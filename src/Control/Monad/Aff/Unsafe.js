/* global console */
"use strict";

exports.unsafeTrace = function (v) {
  return function (success, error) {
    console.log(v);

    try {
      success(v);
    } catch (e) {
      error(e);
    }

    var nonCanceler;

    nonCanceler = function () {
      return function (success) {
        success(false);

        return nonCanceler;
      };
    };

    return nonCanceler;
  };
};

exports.unsafeInterleaveAff = function (aff) {
  return aff;
};
