/* globals setImmediate, clearImmediate, setTimeout, clearTimeout */
/* jshint -W083, -W098 */
"use strict";

var Aff = function () {
  // A unique value for empty.
  var EMPTY = {};

  /*

  An awkward approximation. We elide evidence we would otherwise need in PS for
  efficiency sake.

  data Aff eff a
    = Pure a
    | Throw Error
    | Catch (Aff eff a) (Error -> Aff eff a)
    | Sync (Eff eff a)
    | Async ((Either Error a -> Eff eff Unit) -> Eff eff (Canceler eff))
    | forall b. Bind (Aff eff b) (b -> Aff eff a)
    | forall b. Bracket (Aff eff b) (BracketConditions eff b) (b -> Aff eff a)
    | forall b. Fork Boolean (Aff eff b) ?(Thread eff b -> a)

  */
  var PURE    = "Pure";
  var THROW   = "Throw";
  var CATCH   = "Catch";
  var SYNC    = "Sync";
  var ASYNC   = "Async";
  var BIND    = "Bind";
  var BRACKET = "Bracket";
  var FORK    = "Fork";

  /*

  data ParAff eff a
    = forall b. Map (b -> a) (ParAff eff b)
    | forall b. Apply (ParAff eff (b -> a)) (ParAff eff b)
    | Alt (ParAff eff a) (ParAff eff a)
    | ?Par (Aff eff a)

  */
  var MAP   = "Map";
  var APPLY = "Apply";
  var ALT   = "Alt";

  // Various constructors used in interpretation
  var CONS      = "Cons";      // Cons-list, for stacks
  var RECOVER   = "Recover";   // Continue with error handler
  var RESUME    = "Resume";    // Continue indiscriminately
  var BRACKETED = "Bracketed"; // Continue with bracket finalizers
  var FINALIZED = "Finalized"; // Marker for finalization
  var FORKED    = "Forked";    // Reference to a forked fiber, with resumption stack
  var FIBER     = "Fiber";     // Actual fiber reference
  var THUNK     = "Thunk";     // Primed effect, ready to invoke

  function Aff(tag, _1, _2, _3) {
    this.tag = tag;
    this._1  = _1;
    this._2  = _2;
    this._3  = _3;
  }

  function AffCtr(tag) {
    var fn = function (_1, _2, _3) {
      return new Aff(tag, _1, _2, _3);
    };
    fn.tag = tag;
    return fn;
  }

  function nonCanceler(error) {
    return new Aff(PURE, void 0);
  };

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

  var schedule = function () {
    var limit    = 1024;
    var size     = 0;
    var ix       = 0;
    var queue    = new Array(limit);
    var draining = false;

    return function (cb) {
      var i, thunk;
      if (size === limit) {
        throw new Error("[Aff] Scheduler full");
      }
      queue[(ix + size) % limit] = cb;
      size++;

      if (!draining) {
        draining = true;
        while (size) {
          size--;
          thunk     = queue[ix];
          queue[ix] = void 0;
          ix        = (ix + 1) % limit;
          thunk();
        }
        draining = false;
      }
    };
  }();

  // Fiber state machine
  var SUSPENDED = 0; // Suspended, pending a join.
  var CONTINUE  = 1; // Interpret the next instruction.
  var BINDSTEP  = 2; // Apply the next bind.
  var PENDING   = 3; // An async effect is running.
  var RETURN    = 4; // The current stack has returned.
  var KILLFORKS = 5; // Killing supervised forks.
  var COMPLETED = 6; // The entire fiber has completed.

  function runFiber(util, initStatus, aff, completeCb) {
    // Monotonically increasing tick, increased on each asynchronous turn.
    var runTick = 0;

    // The current branch of the state machine.
    var status = initStatus;

    // The current point of interest for the state machine branch.
    var step      = aff;  // Successful step
    var fail      = null; // Failure step
    var interrupt = null; // Asynchronous interrupt

    // Stack of continuations for the current fiber.
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

    // Track child forks so they don't outlive the parent thread.
    var forkCount = 0;
    var forkId    = 0;
    var forks     = {};

    // Temporary bindings for the various branches.
    var tmp, result, attempt, canceler;

    function launchChildFiber(fid, childStatus, child) {
      forkCount++;
      var blocked = true;
      var fiber = runFiber(util, childStatus, child, function () {
        forkCount--;
        if (blocked) {
          blocked = false;
        } else {
          delete forks[fid];
        }
      });
      if (blocked) {
        blocked = false;
        forks[fid] = fiber;
      }
      return fiber;
    }

    function killChildFibers(finalStep) {
      return new Aff(ASYNC, function (cb) {
        return function () {
          var killError = new Error("[Aff] Child fiber outlived parent");
          var killId    = 0;
          var kills     = {};
          for (var k in forks) {
            if (forks.hasOwnProperty(k)) {
              kills[killId++] = forks[k].kill(killError);
            }
          }
          forks = {};
          forkCount = 0;
          for (var i = 0, len = killId; i < len; i++) {
            kills[i] = runFiber(util, CONTINUE, kills[i], function () {
              delete kills[i];
              killId--;
              if (killId === 0) {
                cb(finalStep)();
              }
            });
          }
          return function (error) {
            return new Aff(SYNC, function () {
              for (var k in kills) {
                if (kills.hasOwnProperty(k)) {
                  runFiber(util, CONTINUE, kills[k].kill(error), function () {});
                }
              }
            });
          };
        };
      });
    }

    // Each invocation of `run` requires a tick. When an asynchronous effect is
    // resolved, we must check that the local tick coincides with the fiber
    // tick before resuming. This prevents multiple async continuations from
    // accidentally resuming the same fiber. A common example may be invoking
    // the provided callback in `makeAff` more than once, but it may also be an
    // async effect resuming after the fiber was already cancelled.
    function run(localRunTick) {
      while (true) {
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
              step   = util.right(step._1);
            } else {
              status = BINDSTEP;
              step   = step._1;
            }
            break;

          case THROW:
            bhead  = null;
            btail  = null;
            status = RETURN;
            fail   = util.left(step._1);
            break;

          case SYNC:
            result = runSync(util.left, util.right, step._1);
            if (util.isLeft(result)) {
              status = RETURN;
              fail   = result;
            } else if (bhead === null) {
              status = RETURN;
              step   = result;
            } else {
              status = BINDSTEP;
              step   = util.fromRight(result);
            }
            break;

          case ASYNC:
            status = PENDING;
            step   = runAsync(util.left, step._1, function (result) {
              return function () {
                if (runTick !== localRunTick) {
                  return;
                } else {
                  runTick++;
                }
                schedule(function () {
                  if (util.isLeft(result)) {
                    status = RETURN;
                    fail   = result;
                  } else if (bhead === null) {
                    status = RETURN;
                    step   = result;
                  } else {
                    status = BINDSTEP;
                    step   = util.fromRight(result);
                  }
                  run(runTick);
                });
              };
            });
            return;

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

          case FORK:
            status = BINDSTEP;
            step   = launchChildFiber(forkId++, step._1, step._2);
            break;
          }
          break;

        case RETURN:
          // If the current stack has returned, and we have no other stacks to
          // resume or finalizers to run, the fiber has halted and we can
          // invoke all join callbacks. Otherwise we need to resume.
          if (attempts === null) {
            runTick++; // Increment the counter to prevent reentry after completion.
            status = KILLFORKS;
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
                  step   = util.fromRight(step);
                } else {
                  status = CONTINUE;
                  step   = attempt._1(util.fromLeft(fail));
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
                step   = util.fromRight(step);
              }
              break;

            // If we have a bracket, we should enqueue the handlers,
            // and continue with the success branch only if the fiber has
            // not been interrupted. If the bracket acquisition failed, we
            // should not run either.
            case BRACKET:
              bracket--;
              if (fail === null) {
                result   = util.fromRight(step);
                attempts = new Aff(CONS, new Aff(BRACKETED, attempt._2, result), attempts._2);
                if (interrupt === null || bracket > 0) {
                  status = CONTINUE;
                  step   = attempt._3(result);
                }
              } else {
                attempts = attempts._2;
              }
              break;

            // Enqueue the appropriate handler. We increase the bracket count
            // because it should be cancelled.
            case BRACKETED:
              bracket++;
              attempts = new Aff(CONS, new Aff(FINALIZED, step), attempts._2);
              status   = CONTINUE;
              if (interrupt !== null) {
                step = attempt._1.killed(util.fromLeft(interrupt))(attempt._2);
              } else if (fail !== null) {
                step = attempt._1.failed(util.fromLeft(fail))(attempt._2);
              } else {
                step = attempt._1.completed(util.fromRight(step))(attempt._2);
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

        case KILLFORKS:
          if (forkCount === 0) {
            status = COMPLETED;
          } else {
            status = CONTINUE;
            step   = killChildFibers(step);
          }
          break;

        case COMPLETED:
          completeCb(step);
          tmp = false;
          for (var k in joins) {
            if ({}.hasOwnProperty.call(joins, k)) {
              tmp = true;
              runEff(joins[k](step));
            }
          }
          joins = tmp;
          // If we have an unhandled exception, and no other fiber has joined
          // then we need to throw the exception in a fresh stack.
          if (util.isLeft(step) && !joins) {
            setTimeout(function () {
              // Guard on joins because a completely synchronous fiber can
              // still have an observer which was added after-the-fact.
              if (!joins) {
                throw util.fromLeft(step);
              }
            }, 0);
          }
          return;
        case SUSPENDED:
          status = CONTINUE;
          break;
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
            return cb(util.right(void 0));
          };
          switch (status) {
          case SUSPENDED:
            status    = COMPLETED;
            interrupt = util.left(error);
            /* fallthrough */
          case COMPLETED:
            canceler = nonCanceler;
            killCb()();
            break;
          case PENDING:
            canceler = addJoinCallback(killCb);
            if (interrupt === null) {
              interrupt = util.left(error);
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
              interrupt = util.left(error);
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

    var join = new Aff(ASYNC, function (cb) {
      return function () {
        var canceler;
        switch (status) {
        case SUSPENDED:
          canceler = addJoinCallback(cb);
          run(runTick);
          break;
        case COMPLETED:
          canceler = nonCanceler;
          joins    = true;
          cb(step)();
          break;
        default:
          canceler = addJoinCallback(cb);
        }
        return canceler;
      };
    });

    if (status === CONTINUE) {
      run(runTick);
    }

    return {
      kill: kill,
      join: join
    };
  }

  function runPar(util, par, cb) {
    // Table of all forked fibers.
    var fiberId   = 0;
    var fibers    = {};

    // Table of currently running cancelers, as a product of `Alt` behavior.
    var killId    = 0;
    var kills     = {};

    // Error used for early cancelation on Alt branches.
    var early     = new Error("[ParAff] Early exit");

    // Error used to kill the entire tree.
    var interrupt = null;

    // The root pointer of the tree.
    var root      = EMPTY;

    // Walks a tree, invoking all the cancelers. Returns the table of pending
    // cancellation fibers.
    function kill(error, par, cb) {
      var step  = par;
      var fail  = null;
      var head  = null;
      var tail  = null;
      var count = 0;
      var kills = {};
      var tmp, kid;

      loop: while (true) {
        tmp = null;

        switch (step.tag) {
        case FORKED:
          tmp = fibers[step._1];
          // If we haven't forked the fiber yet (such as with a sync Alt),
          // then we should just remove it from the queue and continue.
          if (tmp.tag === THUNK) {
            delete fibers[step._1];
            cb(util.right(void 0))();
          } else {
            // Again, we prime the effect but don't run it yet, so that we can
            // collect all the fibers first.
            kills[count++] = function (aff) {
              return function () {
                return runFiber(util, CONTINUE, aff, function (result) {
                  count--;
                  if (fail === null && util.isLeft(result)) {
                    fail = result;
                  }
                  // We can resolve the callback when all fibers have died.
                  if (count === 0) {
                    cb(fail || util.right(void 0))();
                  }
                });
              };
            }(tmp._1.kill(error));
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
      kid = 0;
      tmp = count;
      for (; kid < tmp; kid++) {
        kills[kid] = kills[kid]();
      }

      return kills;
    }

    // When a fiber resolves, we need to bubble back up the tree with the
    // result, computing the applicative nodes.
    function join(result, head, tail) {
      var fail, step, lhs, rhs, tmp, kid;

      if (util.isLeft(result)) {
        fail = result;
        step = null;
      } else {
        step = result;
        fail = null;
      }

      loop: while (true) {
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
            head._3 = util.right(head._1(util.fromRight(step)));
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
          if (util.isLeft(lhs)) {
            if (util.isLeft(rhs)) {
              if (step === lhs) {
                step = rhs;
              }
            } else {
              step = lhs;
            }
          } else if (util.isLeft(rhs)) {
            step = rhs;
          } else {
            head._3 = util.right(util.fromRight(lhs)(util.fromRight(rhs)));
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
              if (util.isLeft(killResult)) {
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

    function resolve(fiber) {
      return function (result) {
        delete fibers[fiber._1];
        fiber._3 = result;
        join(result, fiber._2._1, fiber._2._2);
      };
    }

    // Walks the applicative tree, substituting non-applicative nodes with
    // `FORKED` nodes. In this tree, all applicative nodes use the `_3` slot
    // as a mutable slot for memoization. In an unresolved state, the `_3`
    // slot is `EMPTY`. In the cases of `ALT` and `APPLY`, we always walk
    // the left side first, because both operations are left-associative. As
    // we `RETURN` from those branches, we then walk the right side.
    function run() {
      var status = CONTINUE;
      var step   = par;
      var head   = null;
      var tail   = null;
      var tmp, fid;

      loop: while (true) {
        tmp = null;
        fid = null;

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
            // When we hit a leaf value, we suspend the stack in the `FORKED`.
            // When the fiber resolves, it can bubble back up the tree.
            fid    = fiberId++;
            status = RETURN;
            tmp    = step;
            step   = new Aff(FORKED, fid, new Aff(CONS, head, tail), EMPTY);
            // We prime the effect, but don't immediately run it. We need to
            // walk the entire tree first before actually running effects
            // because they may all be synchronous and resolve immediately, at
            // which point it would attempt to resolve against an incomplete
            // tree.
            fibers[fid] = function (aff, completeCb) {
              return new Aff(THUNK, function () {
                return runFiber(util, CONTINUE, aff, completeCb);
              });
            }(tmp, resolve(step));
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

      // Walk the primed fibers and fork them. We store the actual `Fiber`
      // reference so we can cancel them when needed.
      for (fid = 0; fid < fiberId; fid++) {
        tmp = fibers[fid];
        if (tmp && tmp.tag === THUNK) {
          fibers[fid] = new Aff(FIBER, tmp._1());
        }
      }
    }

    // Cancels the entire tree. If there are already subtrees being canceled,
    // we need to first cancel those joins. This is important so that errors
    // don't accidentally get swallowed by irrelevant join callbacks.
    function cancel(error, cb) {
      interrupt = util.left(error);

      // We can drop the fibers here because we are only canceling join
      // attempts, which are synchronous anyway.
      for (var kid = 0, n = killId; kid < n; kid++) {
        runFiber(util, CONTINUE, kills[kid].kill(error), function () {});
      }

      var newKills = kill(error, root, cb);

      return function (killError) {
        return new Aff(ASYNC, function (killCb) {
          return function () {
            for (var kid in newKills) {
              if (newKills.hasOwnProperty(kid)) {
                runFiber(util, CONTINUE, newKills[kid].kill(killError), function () {});
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

  Aff.EMPTY    = EMPTY;
  Aff.Pure     = AffCtr(PURE);
  Aff.Throw    = AffCtr(THROW);
  Aff.Catch    = AffCtr(CATCH);
  Aff.Sync     = AffCtr(SYNC);
  Aff.Async    = AffCtr(ASYNC);
  Aff.Bind     = AffCtr(BIND);
  Aff.Bracket  = AffCtr(BRACKET);
  Aff.Fork     = AffCtr(FORK);
  Aff.ParMap   = AffCtr(MAP);
  Aff.ParApply = AffCtr(APPLY);
  Aff.ParAlt   = AffCtr(ALT);
  Aff.runFiber = runFiber;
  Aff.runPar   = runPar;

  return Aff;
}();

exports._pure = Aff.Pure;

exports._throwError = Aff.Throw;

exports._catchError = function (aff) {
  return function (k) {
    return Aff.Catch(aff, k);
  };
};

exports._map = function (f) {
  return function (aff) {
    if (aff.tag === Aff.Pure.tag) {
      return Aff.Pure(f(aff._1));
    } else {
      return Aff.Bind(aff, function (value) {
        return Aff.Pure(f(value));
      });
    }
  };
};

exports._bind = function (aff) {
  return function (k) {
    return Aff.Bind(aff, k);
  };
};

exports._fork = function (status) {
  return function (aff) {
    return Aff.Fork(status, aff);
  };
};

exports._liftEff = Aff.Sync;

exports._parAffMap = function (f) {
  return function (aff) {
    return Aff.ParMap(f, aff);
  };
};

exports._parAffApply = function (aff1) {
  return function (aff2) {
    return Aff.ParApply(aff1, aff2);
  };
};

exports._parAffAlt = function (aff1) {
  return function (aff2) {
    return Aff.ParAlt(aff1, aff2);
  };
};

exports.makeAff = Aff.Async;

exports.generalBracket = function (acquire) {
  return function (options) {
    return function (k) {
      return Aff.Bracket(acquire, options, k);
    };
  };
};

exports.memoAff = function (aff) {
  var value = Aff.EMPTY;
  return Aff.Bind(Aff.Pure(void 0), function () {
    if (value === Aff.EMPTY) {
      return Aff.Bind(aff, function (result) {
        value = Aff.Pure(result);
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
    return Aff.Async(function (cb) {
      return function () {
        var timer = setDelay(ms, cb(right()));
        return function () {
          return Aff.Sync(function () {
            return right(clearDelay(ms, timer));
          });
        };
      };
    });
  };
}();

exports._launchAff = function (util, status, aff) {
  return function () {
    return Aff.runFiber(util, status, aff, function () {});
  };
};

exports._sequential = function(util, par) {
  return Aff.Async(function (cb) {
    return function () {
      return Aff.runPar(util, par, cb);
    };
  });
};
