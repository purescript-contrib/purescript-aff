/* global exports */
"use strict";

// module Control.Monad.Aff.AVar

exports._makeVar = function (nonCanceler) {
  return function(success, error) {
    try {
      success({
        consumers: [],
        producers: [],
        error: undefined
      });
    } catch (err) {
      error(err);
    }

    return nonCanceler;
  }
}

exports._takeVar = function (nonCanceler, avar) {
  return function(success, error) {
    if (avar.error !== undefined) {
      error(avar.error);
    } else if (avar.producers.length > 0) {
      var producer = avar.producers.shift();

      producer(success, error);
    } else {
      avar.consumers.push({success: success, error: error});
    }

    return nonCanceler;
  }
}

exports._putVar = function (nonCanceler, avar, a) {
  return function(success, error) {
    if (avar.error !== undefined) {
      error(avar.error);
    } else if (avar.consumers.length === 0) {
      avar.producers.push(function(success, error) {
        try {
          success(a);
        } catch (err) {
          error(err);
        }
      });

      success({});
    } else {
      var consumer = avar.consumers.shift();

      try {
        consumer.success(a);
      } catch (err) {
        error(err);

        return;
      }

      success({});
    }

    return nonCanceler;
  }
}

exports._killVar = function (nonCanceler, avar, e) {
  return function(success, error) {
    if (avar.error !== undefined) {
      error(avar.error);
    } else {
      var errors = [];

      avar.error = e;

      while (avar.consumers.length > 0) {
        var consumer = avar.consumers.shift();

        try {
          consumer.error(e);
        } catch (err) {
          errors.push(err);
        }
      }

      if (errors.length > 0) error(errors[0]);
      else success({});
    }

    return nonCanceler;
  }
}
