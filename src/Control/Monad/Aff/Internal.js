"use strict";

exports._makeVar = function (nonCanceler) {
  return function (success) {
    success({
      consumers: [],
      producers: [],
      error: undefined
    });
    return nonCanceler;
  };
};

exports._takeVar = function (nonCanceler, avar) {
  return function (success, error) {
    if (avar.error !== undefined) {
      error(avar.error);
    } else if (avar.producers.length > 0) {
      avar.producers.shift()(success, error);
    } else {
      avar.consumers.push({ peek: false, success: success, error: error });
    }

    return nonCanceler;
  };
};

exports._peekVar = function (nonCanceler, avar) {
  return function (success, error) {
    if (avar.error !== undefined) {
      error(avar.error);
    } else if (avar.producers.length > 0) {
      avar.producers[0](success, error);
    } else {
      avar.consumers.push({ peek: true, success: success, error: error });
    }
    return nonCanceler;
  };
};

exports._putVar = function (nonCanceler, avar, a) {
  return function (success, error) {
    if (avar.error !== undefined) {
      error(avar.error);
    } else {
      var shouldQueue = true;
      var consumers = [];
      var consumer;

      while (true) {
        consumer = avar.consumers.shift();
        if (consumer) {
          consumers.push(consumer);
          if (consumer.peek) {
            continue;
          } else {
            shouldQueue = false;
          }
        }
        break;
      }

      if (shouldQueue) {
        avar.producers.push(function (success) {
          success(a);
          return nonCanceler;
        });
      }

      for (var i = 0; i < consumers.length; i++) {
        consumers[i].success(a);
      }

      success({});
    }

    return nonCanceler;
  };
};

exports._killVar = function (nonCanceler, avar, e) {
  return function (success, error) {
    if (avar.error !== undefined) {
      error(avar.error);
    } else {
      avar.error = e;
      while (avar.consumers.length) {
        avar.consumers.shift().error(e);
      }
      success({});
    }

    return nonCanceler;
  };
};
