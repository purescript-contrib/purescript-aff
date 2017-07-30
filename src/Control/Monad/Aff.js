/* globals setImmediate, clearImmediate, setTimeout, clearTimeout */
/* jshint -W083, -W098 */
"use strict";

// A unique value for empty.
var EMPTY = {};

/*

An awkward approximation. We elide evidence we would otherwise need in PS for
efficiency sake.

data Aff eff a
  = Pure a
  | Throw Error
  | Sync (Eff eff a)
  | Async ((Either Error a -> Eff eff Unit) -> Eff eff (Canceler eff))
  | forall b. Catch (Error -> a) (Aff eff b) ?(b -> a)
  | forall b. Bracket (Aff eff b) (b -> Aff eff Unit) (b -> Aff eff a)

*/
var PURE    = "Pure";
var THROW   = "Throw";
var SYNC    = "Sync";
var ASYNC   = "Async";
var BIND    = "Bind";
var CATCH   = "Catch";
var BRACKET = "Bracket";

/*

data ParAff eff a
  = forall b. Map (b -> a) (ParAff eff b)
  | forall b. Apply (ParAff eff (b -> a)) (ParAff eff b)
  | Alt (ParAff eff a) (ParAff eff a)
  | Par (Aff eff a)

*/
var MAP   = "Map"
var APPLY = "Apply"
var ALT   = "Alt"

// These are constructors used to implement the recover stack. We still use the
// Aff constructor so that property offsets can always inline.
var CONS      = "Cons";      // Cons-list
var RECOVER   = "Recover";   // Continue with `Either Error a` (via attempt)
var RESUME    = "Resume";    // Continue indiscriminately
var FINALIZED = "Finalized"; // Marker for finalization
var THREAD    = "Thread";

function Aff(tag, _1, _2, _3) {
  this.tag = tag;
  this._1  = _1;
  this._2  = _2;
  this._3  = _3;
}

var nonCanceler = function (error) {
  return new Aff(PURE, void 0);
};

exports._pure = function (a) {
  return new Aff(PURE, a);
};

exports._throwError = function (error) {
  return new Aff(THROW, error);
};

exports._catchError = function (aff) {
  return function (k) {
    return new Aff(CATCH, aff, k);
  };
};

exports._map = function (f) {
  return function (aff) {
    if (aff.tag === PURE) {
      return new Aff(PURE, f(aff._1));
    } else {
      return new Aff(BIND, aff, function (value) {
        return new Aff(PURE, f(value));
      });
    }
  };
};

exports._bind = function (aff) {
  return function (k) {
    return new Aff(BIND, aff, k);
  };
};

exports._liftEff = function (eff) {
  return new Aff(SYNC, eff);
};

exports._parAffMap = function (f) {
  return function (aff) {
    return new Aff(MAP, f, aff);
  };
};

exports._parAffApply = function (aff1) {
  return function (aff2) {
    return new Aff(APPLY, aff1, aff2);
  };
};

exports._parAffAlt = function (aff1) {
  return function (aff2) {
    return new Aff(ALT, aff1, aff2);
  };
};

exports.makeAff = function (k) {
  return new Aff(ASYNC, k);
};

exports.bracket = function (acquire) {
  return function (release) {
    return function (k) {
      return new Aff(BRACKET, acquire, release, k);
    };
  };
};

exports.memoAff = function (aff) {
  var value = EMPTY;
  return new Aff(BIND, new Aff(PURE, void 0), function () {
    if (value === EMPTY) {
      return new Aff(BIND, aff, function (result) {
        value = new Aff(PURE, result);
        return value;
      });
    } else {
      return value;
    }
  });
};

exports._delay = function () {
  function setDelay(n, k) {
    if (n === 0 && typeof setImmediate !== "undefined") {
      return setImmediate(k);
    } else {
      return setTimeout(k, n);
    }
  }

  function clearDelay(n, t) {
    if (n === 0 && typeof clearImmediate !== "undefined") {
      return clearImmediate(t);
    } else {
      return clearTimeout(t);
    }
  }

  return function (right, ms) {
    return new Aff(ASYNC, function (cb) {
      return function () {
        var timer = setDelay(ms, cb(right()));
        return function () {
          return new Aff(SYNC, function () {
            return right(clearDelay(ms, timer));
          });
        };
      };
    });
  };
}();

function runEff(eff) {
  try {
    eff();
  } catch (error) {
    setTimeout(function () {
      throw error;
    }, 0);
  }
}

function runSync(left, right, eff) {
  try {
    return right(eff());
  } catch (error) {
    return left(error);
  }
}

function runAsync(left, eff, k) {
  try {
    return eff(k)();
  } catch (error) {
    k(left(error))();
    return nonCanceler;
  }
}

// Thread state machine
var BLOCKED   = 0; // No effect is running.
var PENDING   = 1; // An async effect is running.
var RETURN    = 2; // The current stack has returned.
var CONTINUE  = 3; // Run the next effect.
var BINDSTEP  = 4; // Apply the next bind.
var COMPLETED = 5; // The entire thread has completed.

exports._launchAff = function (isLeft, fromLeft, fromRight, left, right, aff) {
  return function () {
    // Monotonically increasing tick, increased on each asynchronous turn.
    var runTick = 0;

    // The current branch of the state machine.
    var status = CONTINUE;

    // The current point of interest for the state machine branch.
    var step      = aff;  // Successful step
    var fail      = null; // Failure step
    var interrupt = null; // Asynchronous interrupt

    // Stack of continuations for the current thread.
    var bhead = null;
    var btail = null;

    // Stack of attempts and finalizers for error recovery. This holds a union
    // of an arbitrary Aff finalizer or a Cons list of bind continuations.
    var attempts = null;

    // A special state is needed for Bracket, because it cannot be killed. When
    // we enter a bracket acquisition or finalizer, we increment the counter,
    // and then decrement once complete.
    var bracket = 0;

    // Each join gets a new id so they can be revoked.
    var joinId = 0;
    var joins  = {};

    // Temporary bindings for the various branches.
    var tmp, result, attempt, canceler;

    // Each invocation of `run` requires a tick. When an asynchronous effect is
    // resolved, we must check that the local tick coincides with the thread
    // tick before resuming. This prevents multiple async continuations from
    // accidentally resuming the same thread. A common example may be invoking
    // the provided callback in `makeAff` more than once, but it may also be an
    // async effect resuming after the thread was already cancelled.
    function run(localRunTick) {
      while (1) {
        tmp       = null;
        result    = null;
        attempt   = null;
        canceler  = null;
        switch (status) {
        case BINDSTEP:
          status = CONTINUE;
          step   = bhead(step);
          if (btail === null) {
            bhead = null;
          } else {
            bhead = btail._1;
            btail = btail._2;
          }
          break;

        case CONTINUE:
          switch (step.tag) {
          case BIND:
            if (bhead) {
              btail = new Aff(CONS, bhead, btail);
            }
            bhead  = step._2;
            status = CONTINUE;
            step   = step._1;
            break;

          case PURE:
            if (bhead === null) {
              status = RETURN;
              step   = right(step._1);
            } else {
              status = BINDSTEP;
              step   = step._1;
            }
            break;

          case THROW:
            bhead  = null;
            btail  = null;
            status = RETURN;
            fail   = left(step._1);
            break;

          case SYNC:
            status = BLOCKED;
            result = runSync(left, right, step._1);
            if (isLeft(result)) {
              status = RETURN;
              fail   = result;
            } else if (bhead === null) {
              status = RETURN;
              step   = result;
            } else {
              status = BINDSTEP;
              step   = fromRight(result);
            }
            break;

          case ASYNC:
            status   = BLOCKED;
            canceler = runAsync(left, step._1, function (result) {
              return function () {
                if (runTick !== localRunTick) {
                  return;
                }
                tmp = status;
                if (isLeft(result)) {
                  status = RETURN;
                  fail   = result;
                } else if (bhead === null) {
                  status = RETURN;
                  step   = result;
                } else {
                  status = BINDSTEP;
                  step   = fromRight(result);
                }
                // We only need to invoke `run` if the subsequent block has
                // switch the status to PENDING. Otherwise the callback was
                // resolved synchronously, and the current loop can continue
                // normally.
                if (tmp === PENDING) {
                  run(++runTick);
                } else {
                  localRunTick = ++runTick;
                }
              };
            });
            // If the callback was resolved synchronously, the status will have
            // switched to CONTINUE, and we should not move on to PENDING.
            if (status === BLOCKED) {
              status = PENDING;
              step   = canceler;
            }
            break;

          // Enqueue the current stack of binds and continue
          case CATCH:
            attempts = new Aff(CONS, new Aff(RECOVER, step._2, bhead, btail), attempts);
            bhead    = null;
            btail    = null;
            status   = CONTINUE;
            step     = step._1;
            break;

          // When we evaluate a Bracket, we also enqueue the instruction so we
          // can fullfill it later once we return from the acquisition.
          case BRACKET:
            bracket++;
            if (bhead === null) {
               attempts = new Aff(CONS, step, attempts);
            } else {
               attempts = new Aff(CONS, step, new Aff(CONS, new Aff(RESUME, bhead, btail), attempts));
            }
            bhead  = null;
            btail  = null;
            status = CONTINUE;
            step   = step._1;
            break;
          }
          break;

        case RETURN:
          // If the current stack has returned, and we have no other stacks to
          // resume or finalizers to run, the thread has halted and we can
          // invoke all join callbacks. Otherwise we need to resume.
          if (attempts === null) {
            runTick++; // Increment the counter to prevent reentry after completion.
            status = COMPLETED;
            step   = interrupt || fail || step;
          } else {
            attempt = attempts._1;
            switch (attempt.tag) {
            // We cannot recover from an interrupt. Otherwise we should
            // continue stepping, or run the exception handler if an exception
            // was raised.
            case RECOVER:
              attempts = attempts._2;
              if (interrupt === null) {
                bhead  = attempt._2;
                btail  = attempt._3;
                if (fail === null) {
                  status = BINDSTEP;
                  step   = fromRight(step);
                } else {
                  status = CONTINUE;
                  step   = attempt._1(fromLeft(fail));
                  fail   = null;
                }
              }
              break;

            // We cannot resume from an interrupt or exception.
            case RESUME:
              attempts = attempts._2;
              if (interrupt === null && fail === null) {
                bhead  = attempt._1;
                btail  = attempt._2;
                status = BINDSTEP;
                step   = fromRight(step);
              }
              break;

            // If we have a bracket, we should enqueue the finalizer branch,
            // and continue with the success branch only if the thread has
            // not been interrupted. If the bracket acquisition failed, we
            // should not run either.
            case BRACKET:
              bracket--;
              if (fail === null) {
                result   = fromRight(step);
                attempts = new Aff(CONS, attempt._2(result), attempts._2);
                if (interrupt === null || bracket > 0) {
                  status = CONTINUE;
                  step   = attempt._3(result);
                }
              } else {
                attempts = attempts._2;
              }
              break;

            case FINALIZED:
              bracket--;
              attempts = attempts._2;
              step     = attempt._1;
              break;

            // Otherwise we need to run a finalizer, which cannot be interrupted.
            // We insert a FINALIZED marker to know when we can release it.
            default:
              bracket++;
              attempts._1 = new Aff(FINALIZED, step);
              status      = CONTINUE;
              step        = attempt;
            }
          }
          break;

        case COMPLETED:
          tmp = false;
          for (var k in joins) {
            if ({}.hasOwnProperty.call(joins, k)) {
              tmp = true;
              runEff(joins[k](step));
            }
          }
          joins = tmp;
          // If we have an unhandled exception, and no other thread has joined
          // then we need to throw the exception in a fresh stack.
          if (isLeft(step) && !joins) {
            setTimeout(function () {
              // Guard on joins because a completely synchronous thread can
              // still have an observer.
              if (!joins) {
                throw fromLeft(step);
              }
            }, 0);
          }
          return;
        case BLOCKED: return;
        case PENDING: return;
        }
      }
    }

    function addJoinCallback(cb) {
      var jid    = joinId++;
      joins[jid] = cb;
      return function (error) {
        return new Aff(SYNC, function () {
          delete joins[jid];
        });
      };
    }

    function kill(error) {
      return new Aff(ASYNC, function (cb) {
        return function () {
          // Shadow the canceler binding because it can potentially be
          // clobbered if we call `run`.
          var canceler;
          var killCb = function () {
            return cb(right(void 0));
          };
          switch (status) {
          case COMPLETED:
            canceler = nonCanceler;
            killCb()();
            break;
          case PENDING:
            canceler = addJoinCallback(killCb);
            if (interrupt === null) {
              interrupt = left(error);
            }
            // If we can interrupt the pending action, enqueue the canceler as
            // a non-interruptible finalizer.
            if (bracket === 0) {
              attempts = new Aff(CONS, step(error), attempts);
              bhead    = null;
              btail    = null;
              status   = RETURN;
              step     = null;
              fail     = null;
              run(++runTick);
            }
            break;
          default:
            canceler = addJoinCallback(killCb);
            if (interrupt === null) {
              interrupt = left(error);
            }
            if (bracket === 0) {
              bhead  = null;
              btail  = null;
              status = RETURN;
            }
          }
          return canceler;
        };
      });
    }

    function join() {
      return new Aff(ASYNC, function (cb) {
        return function () {
          if (status === COMPLETED) {
            joins = true;
            cb(step)();
            return nonCanceler;
          }
          return addJoinCallback(cb);
        };
      });
    }

    run(runTick);

    return {
      kill: kill,
      join: join()
    };
  };
};

exports._sequential = function (isLeft, fromLeft, fromRight, left, right, runAff, par) {
  function runParAff(cb) {
    // Table of all forked threads.
    var threadId  = 0;
    var threads   = {};

    // Table of currently running cancelers, as a product of `Alt` behavior.
    var killId    = 0;
    var kills     = {};

    // Error used for early cancelation on Alt branches.
    var early     = new Error("ParAff early exit");

    // Error used to kill the entire tree.
    var interrupt = null;

    // The root pointer of the tree.
    var root      = EMPTY;

    // Walks the applicative tree, substituting non-applicative nodes with
    // `THREAD` nodes. In this tree, all applicative nodes use the `_3` slot
    // as a mutable slot for memoization. In an unresolved state, the `_3`
    // slot is `EMPTY`. In the cases of `ALT` and `APPLY`, we always walk
    // the left side first, because both operations are left-associative. As
    // we `RETURN` from those branches, we then walk the right side.
    function run() {
      var status = CONTINUE;
      var step   = par;
      var head   = null;
      var tail   = null;
      var tmp, tid;

      loop: while (1) {
        tmp = null;
        tid = null;

        switch (status) {
        case CONTINUE:
          switch (step.tag) {
          case MAP:
            if (head) {
              tail = new Aff(CONS, head, tail);
            }
            head = new Aff(MAP, step._1, EMPTY, EMPTY);
            step = step._2;
            break;
          case APPLY:
            if (head) {
              tail = new Aff(CONS, head, tail);
            }
            head = new Aff(APPLY, EMPTY, step._2, EMPTY);
            step = step._1;
            break;
          case ALT:
            if (head) {
              tail = new Aff(CONS, head, tail);
            }
            head = new Aff(ALT, EMPTY, step._2, EMPTY);
            step = step._1;
            break;
          default:
            // When we hit a leaf value, we suspend the stack in the `THREAD`.
            // When the thread resolves, it can bubble back up the tree.
            tid    = threadId++;
            status = RETURN;
            tmp    = step;
            step   = new Aff(THREAD, tid, new Aff(CONS, head, tail), EMPTY);
            // We prime the effect, but don't immediately run it. We need to
            // walk the entire tree first before actually running effects
            // because they may all be synchronous and resolve immediately, at
            // which point it would attempt to resolve against an incomplete
            // tree.
            threads[tid] = runAff(resolve(step))(tmp);
          }
          break;
        case RETURN:
          // Terminal case, we are back at the root.
          if (head === null) {
            break loop;
          }
          // If we are done with the right side, we need to continue down the
          // left. Otherwise we should continue up the stack.
          if (head._1 === EMPTY) {
            head._1 = step;
            status  = CONTINUE;
            step    = head._2;
            head._2 = EMPTY;
          } else {
            head._2 = step;
            step    = head;
            if (tail === null) {
              head  = null;
            } else {
              head  = tail._1;
              tail  = tail._2;
            }
          }
        }
      }

      // Keep a reference to the tree root so it can be cancelled.
      root = step;

      // Walk the primed threads and fork them. We store the actual `Thread`
      // reference so we can cancel them when needed.
      for (tid = 0; tid < threadId; tid++) {
        threads[tid] = threads[tid]();
      }
    }

    function resolve(thread) {
      return function (result) {
        return function () {
          delete threads[thread._1];
          thread._3 = result;
          join(result, thread._2._1, thread._2._2);
        };
      };
    }

    // When a thread resolves, we need to bubble back up the tree with the
    // result, computing the applicative nodes.
    function join(result, head, tail) {
      var fail, step, lhs, rhs, tmp, kid;

      if (isLeft(result)) {
        fail = result;
        step = null;
      } else {
        step = result;
        fail = null;
      }

      loop: while (1) {
        lhs = null;
        rhs = null;
        tmp = null;
        kid = null;

        // We should never continue if the entire tree has been interrupted.
        if (interrupt !== null) {
          return;
        }

        // We've made it all the way to the root of the tree, which means
        // the tree has fully evaluated.
        if (head === null) {
          cb(fail || step)();
          return;
        }

        // The tree has already been computed, so we shouldn't try to do it
        // again. This should never happen.
        // TODO: Remove this?
        if (head._3 !== EMPTY) {
          return;
        }

        switch (head.tag) {
        case MAP:
          if (fail === null) {
            head._3 = right(head._1(fromRight(step)));
            step    = head._3;
          } else {
            head._3 = fail;
          }
          break;
        case APPLY:
          lhs = head._1._3;
          rhs = head._2._3;
          // We can only proceed if both sides have resolved.
          if (lhs === EMPTY || rhs === EMPTY) {
            return;
          }
          // If either side resolve with an error, we should continue with
          // the first error.
          if (isLeft(lhs)) {
            if (isLeft(rhs)) {
              if (step === lhs) {
                step = rhs;
              }
            } else {
              step = lhs;
            }
          } else if (isLeft(rhs)) {
            step = rhs;
          } else {
            head._3 = right(fromRight(lhs)(fromRight(rhs)));
            step    = head._3;
          }
          break;
        case ALT:
          lhs     = head._1._3;
          rhs     = head._2._3;
          head._3 = step;
          tmp     = true;
          kid     = killId++;

          // Once a side has resolved, we need to cancel the side that is still
          // pending before we can continue.
          kills[kid] = kill(early, step === lhs ? head._2 : head._1, function (killResult) {
            return function () {
              delete kills[kid];
              if (isLeft(killResult)) {
                fail = killResult;
                step = null;
              }
              if (tmp) {
                tmp = false;
              } else if (tail === null) {
                join(fail || step, null, null);
              } else {
                join(fail || step, tail._1, tail._2);
              }
            };
          });

          if (tmp) {
            tmp = false;
            return;
          }
          break;
        }

        if (tail === null) {
          head = null;
        } else {
          head = tail._1;
          tail = tail._2;
        }
      }
    }

    // Walks a tree, invoking all the cancelers. Returns the table of pending
    // cancellation threads.
    function kill(error, par, cb) {
      var step  = par;
      var fail  = null;
      var head  = null;
      var tail  = null;
      var count = 0;
      var kills = {};
      var tmp, kid;

      loop: while (1) {
        tmp = null;
        kid = null;

        switch (step.tag) {
        case THREAD:
          tmp = threads[step._1];
          kid = count++;
          if (tmp) {
            // Again, we prime the effect but don't run it yet, so that we can
            // collect all the threads first.
            kills[kid] = runAff(function (result) {
              return function () {
                count--;
                if (fail === null && isLeft(result)) {
                  fail = result;
                }
                // We can resolve the callback when all threads have died.
                if (count === 0) {
                  cb(fail || right(void 0))();
                }
              };
            })(tmp.kill(error));
          }
          // Terminal case.
          if (head === null) {
            break loop;
          }
          // Go down the right side of the tree.
          step = head._2;
          if (tail === null) {
            head = null;
          } else {
            head = tail._1;
            tail = tail._2;
          }
          break;
        case MAP:
          step = step._2;
          break;
        case APPLY:
        case ALT:
          if (head) {
            tail = new Aff(CONS, head, tail);
          }
          head = step;
          step = step._1;
          break;
        }
      }

      // Run the cancelation effects. We alias `count` because it's mutable.
      for (kid = 0, tmp = count; kid < tmp; kid++) {
        kills[kid] = kills[kid]();
      }

      return kills;
    }

    function ignore () {
      return function () {};
    }

    // Cancels the entire tree. If there are already subtrees being canceled,
    // we need to first cancel those joins. This is important so that errors
    // don't accidentally get swallowed by irrelevant join callbacks.
    function cancel(error, cb) {
      interrupt = left(error);

      // We can drop the threads here because we are only canceling join
      // attempts, which are synchronous anyway.
      for (var kid = 0, n = killId; kid < n; kid++) {
        runAff(ignore, kills[kid].kill(error))();
      }

      var newKills = kill(error, root, cb);

      return function (killError) {
        return new Aff(ASYNC, function (killCb) {
          return function () {
            for (var kid in newKills) {
              if (newKills.hasOwnProperty(kid)) {
                runAff(ignore, newKills[kid].kill(killError))();
              }
            }
            return nonCanceler;
          };
        });
      };
    }

    run();

    return function (killError) {
      return new Aff(ASYNC, function (killCb) {
        return function () {
          return cancel(killError, killCb);
        };
      });
    };
  }

  return new Aff(ASYNC, function (cb) {
    return function () {
      return runParAff(cb);
    };
  });
};
