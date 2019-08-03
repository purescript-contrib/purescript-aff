'use strict';

exports.throwAnything = function (x) { return function () { throw x; }; };
