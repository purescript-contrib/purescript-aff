/* globals setTimeout, clearTimeout, setImmediate, clearImmediate */
"use strict";

exports._cancelWith = function (nonCanceler, aff, canceler1) {
  return function (success, error) {
    var canceler2 = aff(success, error);

    return function (e) {
      return function (success, error) {
        var cancellations = 0;
        var result = false;
        var errored = false;

        var s = function (bool) {
          cancellations = cancellations + 1;
          result = result || bool;

          if (cancellations === 2 && !errored) {
            success(result);
          }
        };

        var f = function (err) {
          if (!errored) {
            errored = true;
            error(err);
          }
        };

        canceler2(e)(s, f);
        canceler1(e)(s, f);

        return nonCanceler;
      };
    };
  };
};

exports._setTimeout = function (nonCanceler, millis, aff) {
  var set = setTimeout;
  var clear = clearTimeout;
  if (millis <= 0 && typeof setImmediate === "function") {
    set = setImmediate;
    clear = clearImmediate;
  }
  return function (success, error) {
    var canceler;

    var timeout = set(function () {
      canceler = aff(success, error);
    }, millis);

    return function (e) {
      return function (s, f) {
        if (canceler !== undefined) {
          return canceler(e)(s, f);
        } else {
          clear(timeout);
          s(true);
          return nonCanceler;
        }
      };
    };
  };
};

exports._unsafeInterleaveAff = function (aff) {
  return aff;
};

exports._forkAff = function (nonCanceler, aff) {
  var voidF = function () {};

  return function (success) {
    var canceler = aff(voidF, voidF);
    success(canceler);
    return nonCanceler;
  };
};

exports._forkAll = function (nonCanceler, foldl, affs) {
  var voidF = function () {};

  return function (success) {
    var cancelers = foldl(function (acc) {
      return function (aff) {
        acc.push(aff(voidF, voidF));
        return acc;
      };
    })([])(affs);

    var canceler = function (e) {
      return function (success, error) {
        var cancellations = 0;
        var result        = false;
        var errored       = false;

        var s = function (bool) {
          cancellations = cancellations + 1;
          result        = result || bool;

          if (cancellations === cancelers.length && !errored) {
            success(result);
          }
        };

        var f = function (err) {
          if (!errored) {
            errored = true;
            error(err);
          }
        };

        for (var i = 0; i < cancelers.length; i++) {
          cancelers[i](e)(s, f);
        }

        return nonCanceler;
      };
    };

    success(canceler);
    return nonCanceler;
  };
};

exports._makeAff = function (cb) {
  return function (success, error) {
    try {
      return cb(function (e) {
        return function () {
          error(e);
        };
      })(function (v) {
        return function () {
          success(v);
        };
      })();
    } catch (err) {
      error(err);
    }
  };
};

exports._pure = function (nonCanceler, v) {
  return function (success) {
    success(v);
    return nonCanceler;
  };
};

exports._throwError = function (nonCanceler, e) {
  return function (success, error) {
    error(e);
    return nonCanceler;
  };
};

exports._fmap = function (f, aff) {
  return function (success, error) {
    return aff(function (v) {
      success(f(v));
    }, error);
  };
};

exports._bind = function (alwaysCanceler, aff, f) {
  return function (success, error) {
    var canceler1, canceler2;

    var isCanceled    = false;
    var requestCancel = false;

    var onCanceler = function () {};

    canceler1 = aff(function (v) {
      if (requestCancel) {
        isCanceled = true;

        return alwaysCanceler;
      } else {
        canceler2 = f(v)(success, error);

        onCanceler(canceler2);

        return canceler2;
      }
    }, error);

    return function (e) {
      return function (s, f) {
        requestCancel = true;

        if (canceler2 !== undefined) {
          return canceler2(e)(s, f);
        } else {
          return canceler1(e)(function (bool) {
            if (bool || isCanceled) {
              s(true);
            } else {
              onCanceler = function (canceler) {
                canceler(e)(s, f);
              };
            }
          }, f);
        }
      };
    };
  };
};

exports._attempt = function (Left, Right, aff) {
  return function (success) {
    return aff(function (v) {
      success(Right(v));
    }, function (e) {
      success(Left(e));
    });
  };
};

exports._runAff = function (errorT, successT, aff) {
  // If errorT or successT throw, and an Aff is comprised only of synchronous
  // effects, then it's possible for makeAff/liftEff to accidentally catch
  // it, which may end up rerunning the Aff depending on error recovery
  // behavior. To mitigate this, we observe synchronicity using mutation. If
  // an Aff is observed to be synchronous, we let the stack reset and run the
  // handlers outside of the normal callback flow.
  return function () {
    var status = 0;
    var result, success;

    var canceler = aff(function (v) {
      if (status === 2) {
        successT(v)();
      } else {
        status = 1;
        result = v;
        success = true;
      }
    }, function (e) {
      if (status === 2) {
        errorT(e)();
      } else {
        status = 1;
        result = e;
        success = false;
      }
    });

    if (status === 1) {
      if (success) {
        successT(result)();
      } else {
        errorT(result)();
      }
    } else {
      status = 2;
    }

    return canceler;
  };
};

exports._liftEff = function (nonCanceler, e) {
  return function (success, error) {
    var result;
    try {
      result = e();
    } catch (err) {
      error(err);
      return nonCanceler;
    }

    success(result);
    return nonCanceler;
  };
};

exports._tailRecM = function (isLeft, f, a) {
  return function (success, error) {
    return function go (acc) {
      var result, status, canceler;

      // Observes synchronous effects using a flag.
      //   status = 0 (unresolved status)
      //   status = 1 (synchronous effect)
      //   status = 2 (asynchronous effect)

      var csuccess = function (v) {
        // If the status is still unresolved, we have observed a
        // synchronous effect. Otherwise, the status will be `2`.
        if (status === 0) {
          // Store the result for further synchronous processing.
          result = v;
          status = 1;
        } else {
          // When we have observed an asynchronous effect, we use normal
          // recursion. This is safe because we will be on a new stack.
          if (isLeft(v)) {
            go(v.value0);
          } else {
            success(v.value0);
          }
        }
      };

      while (true) {
        status = 0;
        canceler = f(acc)(csuccess, error);

        // If the status has already resolved to `1` by our Aff handler, then
        // we have observed a synchronous effect. Otherwise it will still be
        // `0`.
        if (status === 1) {
          // When we have observed a synchronous effect, we merely swap out the
          // accumulator and continue the loop, preserving stack.
          if (isLeft(result)) {
            acc = result.value0;
            continue;
          } else {
            success(result.value0);
          }
        } else {
          // If the status has not resolved yet, then we have observed an
          // asynchronous effect.
          status = 2;
        }
        return canceler;
      }

    }(a);
  };
};
