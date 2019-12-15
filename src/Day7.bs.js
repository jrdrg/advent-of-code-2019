// Generated by BUCKLESCRIPT VERSION 5.0.6, PLEASE EDIT WITH CARE
'use strict';

var List = require("bs-platform/lib/js/list.js");
var $$Array = require("bs-platform/lib/js/array.js");
var Block = require("bs-platform/lib/js/block.js");
var Curry = require("bs-platform/lib/js/curry.js");
var Printf = require("bs-platform/lib/js/printf.js");
var $$String = require("bs-platform/lib/js/string.js");
var Readline = require("readline");
var Caml_array = require("bs-platform/lib/js/caml_array.js");
var Caml_int32 = require("bs-platform/lib/js/caml_int32.js");
var Caml_format = require("bs-platform/lib/js/caml_format.js");
var Caml_exceptions = require("bs-platform/lib/js/caml_exceptions.js");

var UnknownInstruction = Caml_exceptions.create("Day7-AdventOfCode2019.UnknownInstruction");

var InvalidMode = Caml_exceptions.create("Day7-AdventOfCode2019.InvalidMode");

var NoOutput = Caml_exceptions.create("Day7-AdventOfCode2019.NoOutput");

var input = $$Array.map(Caml_format.caml_int_of_string, $$Array.map($$String.trim, "3,8,1001,8,10,8,105,1,0,0,21,38,55,72,93,118,199,280,361,442,99999,3,9,1001,9,2,9,1002,9,5,9,101,4,9,9,4,9,99,3,9,1002,9,3,9,1001,9,5,9,1002,9,4,9,4,9,99,3,9,101,4,9,9,1002,9,3,9,1001,9,4,9,4,9,99,3,9,1002,9,4,9,1001,9,4,9,102,5,9,9,1001,9,4,9,4,9,99,3,9,101,3,9,9,1002,9,3,9,1001,9,3,9,102,5,9,9,101,4,9,9,4,9,99,3,9,101,1,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,99,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,101,1,9,9,4,9,99,3,9,101,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,99,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,99,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,99".split(",")));

var createInterface = (
        function (rl) {
            return new Promise((res) => {
                var int = rl.createInterface({
                    input: process.stdin,
                    output: process.stdout,
                    terminal: false
                });
                int.on('line', function (i) {
                    int.close();
                    res(i);
                });
            });
        }
    );

var ReadLine = /* module */[/* createInterface */createInterface];

var rl = Readline;

function getOpcode(num) {
  var op = num % 100;
  var mode1 = (num / 100 | 0) % 10;
  var mode2 = (num / 1000 | 0) % 10;
  var mode3 = (num / 10000 | 0) % 10;
  return /* tuple */[
          op,
          mode1,
          mode2,
          mode3
        ];
}

function getValue(input, addrOrValue, mode) {
  if (mode !== 0) {
    if (mode !== 1) {
      throw [
            InvalidMode,
            mode
          ];
    }
    return addrOrValue;
  } else {
    return Caml_array.caml_array_get(input, addrOrValue);
  }
}

function exec(getInput, state) {
  while(true) {
    var input = state[/* input */2];
    var pc = state[/* pc */0];
    var match = getOpcode(Caml_array.caml_array_get(input, pc));
    var mode2 = match[2];
    var mode1 = match[1];
    var opcode = match[0];
    var exit = 0;
    if (opcode >= 9) {
      if (opcode !== 99) {
        exit = 1;
      } else {
        return Promise.resolve(state);
      }
    } else if (opcode > 0) {
      switch (opcode - 1 | 0) {
        case 0 : 
            var inp1 = Caml_array.caml_array_get(input, pc + 1 | 0);
            var inp2 = Caml_array.caml_array_get(input, pc + 2 | 0);
            var output = Caml_array.caml_array_get(input, pc + 3 | 0);
            var result = getValue(input, inp1, mode1) + getValue(input, inp2, mode2) | 0;
            Caml_array.caml_array_set(input, output, result);
            state[/* pc */0] = state[/* pc */0] + 4 | 0;
            continue ;
        case 1 : 
            var inp1$1 = Caml_array.caml_array_get(input, pc + 1 | 0);
            var inp2$1 = Caml_array.caml_array_get(input, pc + 2 | 0);
            var output$1 = Caml_array.caml_array_get(input, pc + 3 | 0);
            var result$1 = Caml_int32.imul(getValue(input, inp1$1, mode1), getValue(input, inp2$1, mode2));
            Caml_array.caml_array_set(input, output$1, result$1);
            state[/* pc */0] = state[/* pc */0] + 4 | 0;
            continue ;
        case 2 : 
            var addr = Caml_array.caml_array_get(input, pc + 1 | 0);
            return Curry._1(getInput, /* () */0).then((function(input,addr){
                      return function (num) {
                        Caml_array.caml_array_set(input, addr, Caml_format.caml_int_of_string(num));
                        state[/* pc */0] = state[/* pc */0] + 2 | 0;
                        return exec(getInput, state);
                      }
                      }(input,addr)));
        case 3 : 
            var addr$1 = Caml_array.caml_array_get(input, pc + 1 | 0);
            var output$2 = getValue(input, addr$1, mode1);
            state[/* pc */0] = state[/* pc */0] + 2 | 0;
            state[/* output */1] = output$2;
            continue ;
        case 4 : 
            var condition = Caml_array.caml_array_get(input, pc + 1 | 0);
            var addr$2 = Caml_array.caml_array_get(input, pc + 2 | 0);
            if (getValue(input, condition, mode1) !== 0) {
              state[/* pc */0] = getValue(input, addr$2, mode2);
            } else {
              state[/* pc */0] = state[/* pc */0] + 3 | 0;
            }
            continue ;
        case 5 : 
            var condition$1 = Caml_array.caml_array_get(input, pc + 1 | 0);
            var addr$3 = Caml_array.caml_array_get(input, pc + 2 | 0);
            if (getValue(input, condition$1, mode1) === 0) {
              state[/* pc */0] = getValue(input, addr$3, mode2);
            } else {
              state[/* pc */0] = state[/* pc */0] + 3 | 0;
            }
            continue ;
        case 6 : 
            var inp1$2 = Caml_array.caml_array_get(input, pc + 1 | 0);
            var inp2$2 = Caml_array.caml_array_get(input, pc + 2 | 0);
            var addr$4 = Caml_array.caml_array_get(input, pc + 3 | 0);
            if (getValue(input, inp1$2, mode1) < getValue(input, inp2$2, mode2)) {
              Caml_array.caml_array_set(input, addr$4, 1);
            } else {
              Caml_array.caml_array_set(input, addr$4, 0);
            }
            state[/* pc */0] = state[/* pc */0] + 4 | 0;
            continue ;
        case 7 : 
            var inp1$3 = Caml_array.caml_array_get(input, pc + 1 | 0);
            var inp2$3 = Caml_array.caml_array_get(input, pc + 2 | 0);
            var addr$5 = Caml_array.caml_array_get(input, pc + 3 | 0);
            if (getValue(input, inp1$3, mode1) === getValue(input, inp2$3, mode2)) {
              Caml_array.caml_array_set(input, addr$5, 1);
            } else {
              Caml_array.caml_array_set(input, addr$5, 0);
            }
            state[/* pc */0] = state[/* pc */0] + 4 | 0;
            continue ;
        
      }
    } else {
      exit = 1;
    }
    if (exit === 1) {
      throw [
            UnknownInstruction,
            Curry._1(Printf.sprintf(/* Format */[
                      /* String_literal */Block.__(11, [
                          "invalid instruction ",
                          /* Int */Block.__(4, [
                              /* Int_d */0,
                              /* No_padding */0,
                              /* No_precision */0,
                              /* End_of_format */0
                            ])
                        ]),
                      "invalid instruction %d"
                    ]), opcode)
          ];
    }
    
  };
}

function init(i) {
  return /* record */[
          /* pc */0,
          /* output */undefined,
          /* input */i
        ];
}

function getInput(param) {
  return Curry._1(createInterface, rl);
}

var inputState = /* :: */[
  1,
  /* :: */[
    0,
    /* :: */[
      4,
      /* :: */[
        3,
        /* :: */[
          2,
          /* [] */0
        ]
      ]
    ]
  ]
];

console.log(List.length(inputState));

function getInputFromList(inputs, lastOutput) {
  var readInput = /* record */[/* contents */false];
  return (function (param) {
      var match = readInput[0];
      if (match) {
        return Promise.resolve(String(lastOutput));
      } else {
        var inp = List.hd(inputs);
        readInput[0] = true;
        return Promise.resolve(String(inp));
      }
    });
}

function run($staropt$star, instructions, inputs) {
  var output = $staropt$star !== undefined ? $staropt$star : 0;
  if (inputs) {
    var rest = inputs[1];
    return exec(getInputFromList(inputs, output), /* record */[
                  /* pc */0,
                  /* output */undefined,
                  /* input */instructions
                ]).then((function (result) {
                  var match = result[/* output */1];
                  if (match !== undefined) {
                    return run(match, instructions, rest);
                  } else {
                    throw NoOutput;
                  }
                }));
  } else {
    return Promise.resolve(output);
  }
}

function makePhaseCombinations(param) {
  var nums = /* :: */[
    0,
    /* :: */[
      1,
      /* :: */[
        2,
        /* :: */[
          3,
          /* :: */[
            4,
            /* [] */0
          ]
        ]
      ]
    ]
  ];
  return List.concat(List.concat(List.concat(List.concat(List.map((function (i1) {
                                var num2 = List.filter((function (i) {
                                          return i !== i1;
                                        }))(nums);
                                return List.map((function (i2) {
                                              var num3 = List.filter((function (i) {
                                                        if (i !== i1) {
                                                          return i !== i2;
                                                        } else {
                                                          return false;
                                                        }
                                                      }))(num2);
                                              return List.map((function (i3) {
                                                            var num4 = List.filter((function (i) {
                                                                      if (i !== i1 && i !== i2) {
                                                                        return i !== i3;
                                                                      } else {
                                                                        return false;
                                                                      }
                                                                    }))(num3);
                                                            return List.map((function (i4) {
                                                                          var num5 = List.filter((function (i) {
                                                                                    if (i !== i1 && i !== i2 && i !== i3) {
                                                                                      return i !== i4;
                                                                                    } else {
                                                                                      return false;
                                                                                    }
                                                                                  }))(num4);
                                                                          return List.map((function (i5) {
                                                                                        return /* :: */[
                                                                                                i1,
                                                                                                /* :: */[
                                                                                                  i2,
                                                                                                  /* :: */[
                                                                                                    i3,
                                                                                                    /* :: */[
                                                                                                      i4,
                                                                                                      /* :: */[
                                                                                                        i5,
                                                                                                        /* [] */0
                                                                                                      ]
                                                                                                    ]
                                                                                                  ]
                                                                                                ]
                                                                                              ];
                                                                                      }), num5);
                                                                        }), num4);
                                                          }), num3);
                                            }), num2);
                              }), nums)))));
}

function tryCombinations($staropt$star, phaseCombinations) {
  var highest = $staropt$star !== undefined ? $staropt$star : 0;
  if (phaseCombinations) {
    var rest = phaseCombinations[1];
    return run(undefined, input, phaseCombinations[0]).then((function (output) {
                  var h = output > highest ? output : highest;
                  if (output > highest) {
                    console.log(Curry._2(Printf.sprintf(/* Format */[
                                  /* String_literal */Block.__(11, [
                                      "Prev: ",
                                      /* Int */Block.__(4, [
                                          /* Int_d */0,
                                          /* No_padding */0,
                                          /* No_precision */0,
                                          /* String_literal */Block.__(11, [
                                              ", new: ",
                                              /* Int */Block.__(4, [
                                                  /* Int_d */0,
                                                  /* No_padding */0,
                                                  /* No_precision */0,
                                                  /* End_of_format */0
                                                ])
                                            ])
                                        ])
                                    ]),
                                  "Prev: %d, new: %d"
                                ]), highest, output));
                  }
                  return tryCombinations(h, rest);
                }));
  } else {
    return Promise.resolve(highest);
  }
}

var phases = makePhaseCombinations(/* () */0);

console.log("Combinations", List.length(phases));

tryCombinations(undefined, phases).then((function (highest) {
        console.log("Part 1", highest);
        return Promise.resolve(/* () */0);
      }));

exports.UnknownInstruction = UnknownInstruction;
exports.InvalidMode = InvalidMode;
exports.NoOutput = NoOutput;
exports.input = input;
exports.ReadLine = ReadLine;
exports.rl = rl;
exports.getOpcode = getOpcode;
exports.getValue = getValue;
exports.exec = exec;
exports.init = init;
exports.getInput = getInput;
exports.inputState = inputState;
exports.getInputFromList = getInputFromList;
exports.run = run;
exports.makePhaseCombinations = makePhaseCombinations;
exports.tryCombinations = tryCombinations;
exports.phases = phases;
/* input Not a pure module */
