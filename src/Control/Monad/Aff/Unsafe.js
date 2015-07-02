/* global exports */
"use strict";

// module Control.Monad.Aff.Unsafe

exports.unsafeTrace = function (v) {
  return function(success, error) {
    console.log(v);

    try {
      success(v);
    } catch (e) {
      error(e);
    }

    var nonCanceler;

    nonCanceler = function(e) {
      return function(sucess, error) {
        success(false);

        return nonCanceler;
      }
    };

    return nonCanceler;
  };
}

exports.unsafeInterleaveAff = function (aff) {
  return aff;
}
