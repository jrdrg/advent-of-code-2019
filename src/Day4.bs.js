// Generated by BUCKLESCRIPT VERSION 5.0.6, PLEASE EDIT WITH CARE
'use strict';

var List = require("bs-platform/lib/js/list.js");
var Block = require("bs-platform/lib/js/block.js");
var Curry = require("bs-platform/lib/js/curry.js");
var Printf = require("bs-platform/lib/js/printf.js");

function hasOnlyTwoAdjacentIdenticalDigits(_next, _digitCount, _n) {
  while(true) {
    var n = _n;
    var digitCount = _digitCount;
    var next = _next;
    var currentDigit = n % 10;
    var exit = 0;
    var digitCountMap;
    var exit$1 = 0;
    if (next !== undefined || digitCount === undefined) {
      exit$1 = 3;
    } else {
      digitCountMap = digitCount;
      exit = 1;
    }
    if (exit$1 === 3) {
      if (n !== 0) {
        if (next !== undefined) {
          var num = next;
          if (digitCount !== undefined) {
            var digitCountMap$1 = digitCount;
            if (num === currentDigit) {
              var tmp;
              if (digitCountMap$1 && num === currentDigit) {
                var match = digitCountMap$1[0];
                tmp = /* :: */[
                  /* tuple */[
                    match[0],
                    match[1] + 1 | 0
                  ],
                  digitCountMap$1[1]
                ];
              } else {
                tmp = /* :: */[
                  /* tuple */[
                    currentDigit,
                    1
                  ],
                  /* [] */0
                ];
              }
              _n = n / 10 | 0;
              _digitCount = tmp;
              _next = currentDigit;
              continue ;
            } else {
              var counts;
              if (digitCountMap$1) {
                var match$1 = digitCountMap$1[0];
                var digit = match$1[0];
                var exit$2 = 0;
                if (match$1[1] !== 2 || digit !== currentDigit) {
                  exit$2 = 4;
                } else {
                  counts = /* :: */[
                    /* tuple */[
                      digit,
                      2
                    ],
                    digitCountMap$1[1]
                  ];
                }
                if (exit$2 === 4) {
                  counts = digit === currentDigit ? /* :: */[
                      /* tuple */[
                        digit,
                        1
                      ],
                      digitCountMap$1[1]
                    ] : /* :: */[
                      /* tuple */[
                        currentDigit,
                        1
                      ],
                      digitCountMap$1
                    ];
                }
                
              } else {
                counts = /* :: */[
                  /* tuple */[
                    currentDigit,
                    1
                  ],
                  digitCountMap$1
                ];
              }
              _n = n / 10 | 0;
              _digitCount = counts;
              _next = currentDigit;
              continue ;
            }
          } else if (num === currentDigit) {
            var counts_000 = /* tuple */[
              currentDigit,
              2
            ];
            var counts$1 = /* :: */[
              counts_000,
              /* [] */0
            ];
            _n = n / 10 | 0;
            _digitCount = counts$1;
            _next = currentDigit;
            continue ;
          } else {
            exit = 2;
          }
        } else {
          exit = 2;
        }
      } else if (digitCount !== undefined) {
        digitCountMap = digitCount;
        exit = 1;
      } else {
        return false;
      }
    }
    switch (exit) {
      case 1 : 
          return List.exists((function (param) {
                        return param[1] === 2;
                      }), digitCountMap);
      case 2 : 
          _n = n / 10 | 0;
          _digitCount = /* :: */[
            /* tuple */[
              currentDigit,
              1
            ],
            /* [] */0
          ];
          _next = currentDigit;
          continue ;
      
    }
  };
}

function hasTwoAdjacentIdenticalDigits(_next, _n) {
  while(true) {
    var n = _n;
    var next = _next;
    var currentDigit = n % 10;
    if (n !== 0) {
      if (next !== undefined) {
        if (next === currentDigit) {
          return true;
        } else {
          _n = n / 10 | 0;
          _next = currentDigit;
          continue ;
        }
      } else {
        _n = n / 10 | 0;
        _next = currentDigit;
        continue ;
      }
    } else {
      return false;
    }
  };
}

function areDigitsNeverDecreasing(_next, _n) {
  while(true) {
    var n = _n;
    var next = _next;
    var currentDigit = n % 10;
    if (n !== 0) {
      if (next !== undefined) {
        if (next < currentDigit) {
          return false;
        } else {
          _n = n / 10 | 0;
          _next = currentDigit;
          continue ;
        }
      } else {
        _n = n / 10 | 0;
        _next = currentDigit;
        continue ;
      }
    } else {
      return true;
    }
  };
}

function matchesCriteria(matcher, n) {
  if (Curry._1(matcher, n)) {
    return areDigitsNeverDecreasing(undefined, n);
  } else {
    return false;
  }
}

function findPasswords(matcher, _current, last, _count) {
  while(true) {
    var count = _count;
    var current = _current;
    if (current >= last) {
      return count;
    } else {
      var match = matchesCriteria(matcher, current);
      var count$1 = match ? count + 1 | 0 : count;
      _count = count$1;
      _current = current + 1 | 0;
      continue ;
    }
  };
}

var part1 = findPasswords((function (eta) {
        return hasTwoAdjacentIdenticalDigits(undefined, eta);
      }), 245318, 765747, 0);

console.log(Curry._1(Printf.sprintf(/* Format */[
              /* String_literal */Block.__(11, [
                  "Part 1: ",
                  /* Int */Block.__(4, [
                      /* Int_d */0,
                      /* No_padding */0,
                      /* No_precision */0,
                      /* End_of_format */0
                    ])
                ]),
              "Part 1: %d"
            ]), part1));

var part2 = findPasswords((function (eta) {
        return hasOnlyTwoAdjacentIdenticalDigits(undefined, undefined, eta);
      }), 245318, 765747, 0);

console.log(Curry._1(Printf.sprintf(/* Format */[
              /* String_literal */Block.__(11, [
                  "Part 2: ",
                  /* Int */Block.__(4, [
                      /* Int_d */0,
                      /* No_padding */0,
                      /* No_precision */0,
                      /* End_of_format */0
                    ])
                ]),
              "Part 2: %d"
            ]), part2));

var startRange = 245318;

var endRange = 765747;

var size = 6;

exports.startRange = startRange;
exports.endRange = endRange;
exports.size = size;
exports.hasOnlyTwoAdjacentIdenticalDigits = hasOnlyTwoAdjacentIdenticalDigits;
exports.hasTwoAdjacentIdenticalDigits = hasTwoAdjacentIdenticalDigits;
exports.areDigitsNeverDecreasing = areDigitsNeverDecreasing;
exports.matchesCriteria = matchesCriteria;
exports.findPasswords = findPasswords;
exports.part1 = part1;
exports.part2 = part2;
/* part1 Not a pure module */
