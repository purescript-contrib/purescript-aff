"use strict";

/*

An awkward approximation. We elide evidence we would otherwise need in PS for
efficiency sake.

data Aff eff a
  = Pure a
  | Throw Error
  | Sync (Eff eff (Either Error a))
  | Async ((Either Error a -> Eff eff Unit) -> Eff eff (Canceler eff))
  | forall b. Attempt (Aff eff b) ?(Either Error b -> a)
  | forall b. Bracket (Aff eff b) (b -> Aff eff Unit) (b -> Aff eff a)

*/
var PURE      = "Pure";
var THROW     = "Throw";
var SYNC      = "Sync";
var ASYNC     = "Async";
var BIND      = "Bind";
var ATTEMPT   = "Attempt";
var BRACKET   = "Bracket";

// These are constructors used to implement the recover stack. We still use the
// Aff constructor so that property offsets can always inline.
var CONS      = "Cons";      // Cons-list
var RECOVER   = "Recover";   // Continue with `Either Error a` (via attempt)
var RESUME    = "Resume";    // Continue indiscriminately
var FINALIZED = "Finalized"; // Marker for finalization

function Aff (tag, _1, _2, _3) {
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

exports.unsafeLiftEff = function (eff) {
  return new Aff(SYNC, eff);
};

exports.unsafeMakeAff = function (k) {
  return new Aff(ASYNC, k);
};

exports.attempt = function (aff) {
  return new Aff(ATTEMPT, aff);
};

exports.bracket = function (acquire) {
  return function (release) {
    return function (k) {
      return new Aff(BRACKET, acquire, release, k);
    };
  };
};

exports._liftEff = function (left, right, eff) {
  return new Aff(SYNC, function () {
    try {
      return right(eff());
    } catch (error) {
      return left(error);
    }
  });
};

exports._makeAff = function (left, right, aff) {
  return new Aff(ASYNC, function (k) {
    return function () {
      try {
        return aff(k)();
      } catch (error) {
        k(left(error))();
        return nonCanceler;
      }
    };
  });
};

exports._delay = function () {
  var setDelay = function (n, k) {
    if (n === 0 && typeof setImmediate !== "undefined") {
      return setImmediate(k);
    } else {
      return setTimeout(k, n);
    }
  };
  var clearDelay = function (n, t) {
    if (n === 0 && typeof clearImmediate !== "undefined") {
      return clearImmediate(t);
    } else {
      return clearTimeout(t);
    }
  };
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
    function run (localRunTick) {
      while (1) {
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
            result = step._1();
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
            canceler = step._1(function (result) {
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
            })();
            // If the callback was resolved synchronously, the status will have
            // switched to CONTINUE, and we should not move on to PENDING.
            if (status === BLOCKED) {
              status = PENDING;
              step   = canceler;
            }
            break;

          // Enqueue the current stack of binds and continue
          case ATTEMPT:
            attempts = new Aff(CONS, new Aff(RECOVER, bhead, btail), attempts);
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
            // We cannot recover from an interrupt. If we are able to recover
            // we should step directly (since the return value is an Either).
            case RECOVER:
              attempts = attempts._2;
              if (interrupt === null) {
                bhead  = attempt._1;
                btail  = attempt._2;
                status = BINDSTEP;
                step   = fail || step;
                fail   = null;
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
            tmp = true;
            runJoin(step, joins[k]);
          }
          joins = tmp;
          // If we have an unhandled exception, and no other thread has joined
          // then we need to throw the exception in a fresh stack.
          if (isLeft(step) && !joins) {
            setTimeout(function() {
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

        tmp       = null;
        result    = null;
        attempt   = null;
        canceler  = null;
      }
    }

    function addJoinCallback (cb) {
      var jid    = joinId++;
      joins[jid] = cb;
      return function (error) {
        return new Aff(SYNC, function () {
          delete joins[jid];
          return right();
        });
      };
    }

    function kill (error) {
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
              run(runTick++);
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

    function join () {
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

function runJoin (result, cb) {
  try {
    cb(result)();
  } catch (error) {
    setTimeout(function () {
      throw error;
    }, 0)
  }
}
