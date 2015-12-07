// module Data.Rational

var bigInt = require("big-integer");

exports.signum = function(n) {
  return bigInt(n.compare(0));
};

exports.js_gcd = bigInt.gcd;
