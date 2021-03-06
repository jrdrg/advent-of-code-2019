// Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE
'use strict';

var $$Map = require("bs-platform/lib/js/map.js");
var $$Array = require("bs-platform/lib/js/array.js");
var Block = require("bs-platform/lib/js/block.js");
var Curry = require("bs-platform/lib/js/curry.js");
var Int64 = require("bs-platform/lib/js/int64.js");
var Printf = require("bs-platform/lib/js/printf.js");
var Readline = require("readline");
var Caml_int64 = require("bs-platform/lib/js/caml_int64.js");
var Caml_format = require("bs-platform/lib/js/caml_format.js");
var Caml_exceptions = require("bs-platform/lib/js/caml_exceptions.js");
var Caml_builtin_exceptions = require("bs-platform/lib/js/caml_builtin_exceptions.js");

var UnknownInstruction = Caml_exceptions.create("Day9-AdventOfCode2019.UnknownInstruction");

var InvalidMode = Caml_exceptions.create("Day9-AdventOfCode2019.InvalidMode");

var input = $$Array.map(Caml_format.caml_int64_of_string, "1102,34463338,34463338,63,1007,63,34463338,63,1005,63,53,1101,3,0,1000,109,988,209,12,9,1000,209,6,209,3,203,0,1008,1000,1,63,1005,63,65,1008,1000,2,63,1005,63,902,1008,1000,0,63,1005,63,58,4,25,104,0,99,4,0,104,0,99,4,17,104,0,99,0,0,1101,26,0,1015,1101,29,0,1010,1102,1,24,1013,1102,1,33,1008,1102,36,1,1012,1101,0,572,1023,1101,35,0,1014,1101,0,38,1019,1102,1,30,1006,1101,0,890,1029,1101,34,0,1011,1101,28,0,1002,1102,1,1,1021,1101,0,37,1001,1101,0,197,1026,1101,22,0,1017,1102,1,895,1028,1101,0,20,1007,1102,21,1,1004,1102,1,39,1016,1101,0,0,1020,1102,1,190,1027,1101,0,775,1024,1102,31,1,1018,1101,0,23,1003,1101,0,25,1009,1101,770,0,1025,1101,0,27,1000,1102,1,575,1022,1101,0,32,1005,109,27,2106,0,0,1001,64,1,64,1106,0,199,4,187,1002,64,2,64,109,-18,21101,40,0,5,1008,1014,39,63,1005,63,219,1106,0,225,4,205,1001,64,1,64,1002,64,2,64,109,-6,1201,-1,0,63,1008,63,28,63,1005,63,251,4,231,1001,64,1,64,1105,1,251,1002,64,2,64,109,5,21102,41,1,3,1008,1011,38,63,1005,63,271,1105,1,277,4,257,1001,64,1,64,1002,64,2,64,109,-7,2102,1,1,63,1008,63,28,63,1005,63,299,4,283,1106,0,303,1001,64,1,64,1002,64,2,64,109,-7,1207,10,22,63,1005,63,321,4,309,1106,0,325,1001,64,1,64,1002,64,2,64,109,16,2107,31,-4,63,1005,63,345,1001,64,1,64,1105,1,347,4,331,1002,64,2,64,109,-9,1201,3,0,63,1008,63,18,63,1005,63,371,1001,64,1,64,1106,0,373,4,353,1002,64,2,64,109,7,1202,-7,1,63,1008,63,40,63,1005,63,393,1106,0,399,4,379,1001,64,1,64,1002,64,2,64,109,-5,1208,5,33,63,1005,63,417,4,405,1106,0,421,1001,64,1,64,1002,64,2,64,109,1,1202,2,1,63,1008,63,30,63,1005,63,443,4,427,1105,1,447,1001,64,1,64,1002,64,2,64,109,-7,2102,1,10,63,1008,63,19,63,1005,63,471,1001,64,1,64,1105,1,473,4,453,1002,64,2,64,109,6,2108,21,0,63,1005,63,489,1105,1,495,4,479,1001,64,1,64,1002,64,2,64,109,9,21108,42,42,0,1005,1012,513,4,501,1105,1,517,1001,64,1,64,1002,64,2,64,109,7,21107,43,44,-1,1005,1018,535,4,523,1106,0,539,1001,64,1,64,1002,64,2,64,109,-5,21101,44,0,2,1008,1016,44,63,1005,63,561,4,545,1105,1,565,1001,64,1,64,1002,64,2,64,2105,1,9,1106,0,581,4,569,1001,64,1,64,1002,64,2,64,109,13,21107,45,44,-9,1005,1018,597,1105,1,603,4,587,1001,64,1,64,1002,64,2,64,109,-25,2101,0,3,63,1008,63,32,63,1005,63,625,4,609,1105,1,629,1001,64,1,64,1002,64,2,64,109,7,1208,-7,30,63,1005,63,645,1105,1,651,4,635,1001,64,1,64,1002,64,2,64,109,-2,21102,46,1,9,1008,1016,46,63,1005,63,677,4,657,1001,64,1,64,1106,0,677,1002,64,2,64,109,-2,21108,47,48,9,1005,1014,697,1001,64,1,64,1105,1,699,4,683,1002,64,2,64,109,14,1205,2,713,4,705,1105,1,717,1001,64,1,64,1002,64,2,64,109,-7,1206,8,735,4,723,1001,64,1,64,1106,0,735,1002,64,2,64,109,-18,2101,0,6,63,1008,63,24,63,1005,63,759,1001,64,1,64,1106,0,761,4,741,1002,64,2,64,109,29,2105,1,1,4,767,1106,0,779,1001,64,1,64,1002,64,2,64,109,-5,1206,3,791,1106,0,797,4,785,1001,64,1,64,1002,64,2,64,109,-12,2107,31,-1,63,1005,63,819,4,803,1001,64,1,64,1105,1,819,1002,64,2,64,109,7,1205,7,835,1001,64,1,64,1105,1,837,4,825,1002,64,2,64,109,-11,1207,7,24,63,1005,63,853,1106,0,859,4,843,1001,64,1,64,1002,64,2,64,109,4,2108,27,-6,63,1005,63,881,4,865,1001,64,1,64,1106,0,881,1002,64,2,64,109,24,2106,0,-2,4,887,1106,0,899,1001,64,1,64,4,64,99,21102,27,1,1,21101,0,913,0,1106,0,920,21201,1,61934,1,204,1,99,109,3,1207,-2,3,63,1005,63,962,21201,-2,-1,1,21101,0,940,0,1106,0,920,21202,1,1,-1,21201,-2,-3,1,21101,0,955,0,1105,1,920,22201,1,-1,-2,1105,1,966,22102,1,-2,-2,109,-3,2105,1,0".split(","));

var Int64Map = $$Map.Make({
      compare: Int64.compare
    });

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

var ReadLine = {
  createInterface: createInterface
};

var rl = Readline;

var $percent$bang = Caml_int64.mod_;

var $slash$bang = Caml_int64.div;

var $plus$bang = Caml_int64.add;

var $star$bang = Caml_int64.mul;

function getOpcode(num) {
  var op = Caml_int64.mod_(num, /* int64 */[
          /* hi */0,
          /* lo */100
        ])[1] | 0;
  var mode1 = Caml_int64.mod_(Caml_int64.div(num, /* int64 */[
              /* hi */0,
              /* lo */100
            ]), /* int64 */[
          /* hi */0,
          /* lo */10
        ])[1] | 0;
  var mode2 = Caml_int64.mod_(Caml_int64.div(num, /* int64 */[
              /* hi */0,
              /* lo */1000
            ]), /* int64 */[
          /* hi */0,
          /* lo */10
        ])[1] | 0;
  var mode3 = Caml_int64.mod_(Caml_int64.div(num, /* int64 */[
              /* hi */0,
              /* lo */10000
            ]), /* int64 */[
          /* hi */0,
          /* lo */10
        ])[1] | 0;
  return /* tuple */[
          op,
          mode1,
          mode2,
          mode3
        ];
}

function setValue(state, addr, value) {
  state[/* extraMemory */3] = Curry._3(Int64Map.add, addr, value, state[/* extraMemory */3]);
  return /* () */0;
}

function readValue(state, addr) {
  try {
    return Curry._2(Int64Map.find, addr, state[/* extraMemory */3]);
  }
  catch (exn){
    if (exn === Caml_builtin_exceptions.not_found) {
      console.log(Curry._1(Printf.sprintf(/* Format */[
                    /* String_literal */Block.__(11, [
                        "Memory not found at ",
                        /* Int64 */Block.__(7, [
                            /* Int_d */0,
                            /* No_padding */0,
                            /* No_precision */0,
                            /* End_of_format */0
                          ])
                      ]),
                    "Memory not found at %Ld"
                  ]), addr));
      return /* int64 */[
              /* hi */0,
              /* lo */0
            ];
    } else {
      throw exn;
    }
  }
}

function writeValue(state, addr, value, mode) {
  if (mode !== 2) {
    return setValue(state, addr, value);
  } else {
    var addr$1 = Caml_int64.add(addr, state[/* relativeBase */1]);
    return setValue(state, addr$1, value);
  }
}

function getValue(state, addrOrValue, mode) {
  var relativeBase = state[/* relativeBase */1];
  switch (mode) {
    case 0 :
        return readValue(state, addrOrValue);
    case 1 :
        return addrOrValue;
    case 2 :
        var addr = Caml_int64.add(addrOrValue, relativeBase);
        return readValue(state, addr);
    default:
      throw [
            InvalidMode,
            mode
          ];
  }
}

function exec(state) {
  while(true) {
    var pc = state[/* pc */0];
    var match = getOpcode(readValue(state, pc));
    var mode3 = match[3];
    var mode2 = match[2];
    var mode1 = match[1];
    var opcode = match[0];
    if (opcode >= 10) {
      if (opcode === 99) {
        return Promise.resolve(state);
      }
      
    } else if (opcode > 0) {
      switch (opcode - 1 | 0) {
        case 0 :
            var inp1 = readValue(state, Caml_int64.add(pc, /* int64 */[
                      /* hi */0,
                      /* lo */1
                    ]));
            var inp2 = readValue(state, Caml_int64.add(pc, /* int64 */[
                      /* hi */0,
                      /* lo */2
                    ]));
            var output = readValue(state, Caml_int64.add(pc, /* int64 */[
                      /* hi */0,
                      /* lo */3
                    ]));
            var result = Caml_int64.add(getValue(state, inp1, mode1), getValue(state, inp2, mode2));
            writeValue(state, output, result, mode3);
            state[/* pc */0] = Caml_int64.add(state[/* pc */0], /* int64 */[
                  /* hi */0,
                  /* lo */4
                ]);
            continue ;
        case 1 :
            var inp1$1 = readValue(state, Caml_int64.add(pc, /* int64 */[
                      /* hi */0,
                      /* lo */1
                    ]));
            var inp2$1 = readValue(state, Caml_int64.add(pc, /* int64 */[
                      /* hi */0,
                      /* lo */2
                    ]));
            var output$1 = readValue(state, Caml_int64.add(pc, /* int64 */[
                      /* hi */0,
                      /* lo */3
                    ]));
            var result$1 = Caml_int64.mul(getValue(state, inp1$1, mode1), getValue(state, inp2$1, mode2));
            writeValue(state, output$1, result$1, mode3);
            state[/* pc */0] = Caml_int64.add(state[/* pc */0], /* int64 */[
                  /* hi */0,
                  /* lo */4
                ]);
            continue ;
        case 2 :
            var addr = readValue(state, Caml_int64.add(pc, /* int64 */[
                      /* hi */0,
                      /* lo */1
                    ]));
            console.log("Input: ");
            return Curry._1(createInterface, rl).then((function(mode1,addr){
                      return function (num) {
                        writeValue(state, addr, Caml_format.caml_int64_of_string(num), mode1);
                        state[/* pc */0] = Caml_int64.add(state[/* pc */0], /* int64 */[
                              /* hi */0,
                              /* lo */2
                            ]);
                        return exec(state);
                      }
                      }(mode1,addr)));
        case 3 :
            var addr$1 = readValue(state, Caml_int64.add(pc, /* int64 */[
                      /* hi */0,
                      /* lo */1
                    ]));
            var output$2 = getValue(state, addr$1, mode1);
            console.log("Out: ", Int64.to_string(output$2));
            state[/* pc */0] = Caml_int64.add(state[/* pc */0], /* int64 */[
                  /* hi */0,
                  /* lo */2
                ]);
            continue ;
        case 4 :
            var condition = readValue(state, Caml_int64.add(pc, /* int64 */[
                      /* hi */0,
                      /* lo */1
                    ]));
            var addr$2 = readValue(state, Caml_int64.add(pc, /* int64 */[
                      /* hi */0,
                      /* lo */2
                    ]));
            var cmpValue = getValue(state, condition, mode1);
            if (Caml_int64.neq(cmpValue, /* int64 */[
                    /* hi */0,
                    /* lo */0
                  ])) {
              state[/* pc */0] = getValue(state, addr$2, mode2);
            } else {
              state[/* pc */0] = Caml_int64.add(state[/* pc */0], /* int64 */[
                    /* hi */0,
                    /* lo */3
                  ]);
            }
            continue ;
        case 5 :
            var condition$1 = readValue(state, Caml_int64.add(pc, /* int64 */[
                      /* hi */0,
                      /* lo */1
                    ]));
            var addr$3 = readValue(state, Caml_int64.add(pc, /* int64 */[
                      /* hi */0,
                      /* lo */2
                    ]));
            var cmpValue$1 = getValue(state, condition$1, mode1);
            if (Caml_int64.eq(cmpValue$1, /* int64 */[
                    /* hi */0,
                    /* lo */0
                  ])) {
              state[/* pc */0] = getValue(state, addr$3, mode2);
            } else {
              state[/* pc */0] = Caml_int64.add(state[/* pc */0], /* int64 */[
                    /* hi */0,
                    /* lo */3
                  ]);
            }
            continue ;
        case 6 :
            var inp1$2 = readValue(state, Caml_int64.add(pc, /* int64 */[
                      /* hi */0,
                      /* lo */1
                    ]));
            var inp2$2 = readValue(state, Caml_int64.add(pc, /* int64 */[
                      /* hi */0,
                      /* lo */2
                    ]));
            var addr$4 = readValue(state, Caml_int64.add(pc, /* int64 */[
                      /* hi */0,
                      /* lo */3
                    ]));
            var v1 = getValue(state, inp1$2, mode1);
            var v2 = getValue(state, inp2$2, mode2);
            if (Caml_int64.lt(v1, v2)) {
              writeValue(state, addr$4, /* int64 */[
                    /* hi */0,
                    /* lo */1
                  ], mode3);
            } else {
              writeValue(state, addr$4, /* int64 */[
                    /* hi */0,
                    /* lo */0
                  ], mode3);
            }
            state[/* pc */0] = Caml_int64.add(state[/* pc */0], /* int64 */[
                  /* hi */0,
                  /* lo */4
                ]);
            continue ;
        case 7 :
            var inp1$3 = readValue(state, Caml_int64.add(pc, /* int64 */[
                      /* hi */0,
                      /* lo */1
                    ]));
            var inp2$3 = readValue(state, Caml_int64.add(pc, /* int64 */[
                      /* hi */0,
                      /* lo */2
                    ]));
            var addr$5 = readValue(state, Caml_int64.add(pc, /* int64 */[
                      /* hi */0,
                      /* lo */3
                    ]));
            var v1$1 = getValue(state, inp1$3, mode1);
            var v2$1 = getValue(state, inp2$3, mode2);
            if (Caml_int64.eq(v1$1, v2$1)) {
              writeValue(state, addr$5, /* int64 */[
                    /* hi */0,
                    /* lo */1
                  ], mode3);
            } else {
              writeValue(state, addr$5, /* int64 */[
                    /* hi */0,
                    /* lo */0
                  ], mode3);
            }
            state[/* pc */0] = Caml_int64.add(state[/* pc */0], /* int64 */[
                  /* hi */0,
                  /* lo */4
                ]);
            continue ;
        case 8 :
            var param = readValue(state, Caml_int64.add(pc, /* int64 */[
                      /* hi */0,
                      /* lo */1
                    ]));
            var relativeBase = getValue(state, param, mode1);
            state[/* relativeBase */1] = Caml_int64.add(state[/* relativeBase */1], relativeBase);
            console.log(Curry._1(Printf.sprintf(/* Format */[
                          /* String_literal */Block.__(11, [
                              "Updating relative base to ",
                              /* Int64 */Block.__(7, [
                                  /* Int_d */0,
                                  /* No_padding */0,
                                  /* No_precision */0,
                                  /* End_of_format */0
                                ])
                            ]),
                          "Updating relative base to %Ld"
                        ]), state[/* relativeBase */1]));
            state[/* pc */0] = Caml_int64.add(state[/* pc */0], /* int64 */[
                  /* hi */0,
                  /* lo */2
                ]);
            continue ;
        
      }
    }
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
  };
}

function make(input) {
  var match = $$Array.fold_left((function (param, item) {
          var index = param[1];
          return /* tuple */[
                  Curry._3(Int64Map.add, index, item, param[0]),
                  Caml_int64.add(index, /* int64 */[
                        /* hi */0,
                        /* lo */1
                      ])
                ];
        }), /* tuple */[
        Int64Map.empty,
        /* int64 */[
          /* hi */0,
          /* lo */0
        ]
      ], input);
  return /* record */[
          /* pc : int64 */[
            /* hi */0,
            /* lo */0
          ],
          /* relativeBase : int64 */[
            /* hi */0,
            /* lo */0
          ],
          /* input */input,
          /* extraMemory */match[0]
        ];
}

exec(make(input)).then((function (param) {
          console.log("Done");
          return Promise.resolve(/* () */0);
        })).catch((function (err) {
        console.log(err);
        return Promise.resolve(/* () */0);
      }));

var l2s = Int64.to_string;

exports.UnknownInstruction = UnknownInstruction;
exports.InvalidMode = InvalidMode;
exports.l2s = l2s;
exports.input = input;
exports.Int64Map = Int64Map;
exports.ReadLine = ReadLine;
exports.rl = rl;
exports.$percent$bang = $percent$bang;
exports.$slash$bang = $slash$bang;
exports.$plus$bang = $plus$bang;
exports.$star$bang = $star$bang;
exports.getOpcode = getOpcode;
exports.setValue = setValue;
exports.readValue = readValue;
exports.writeValue = writeValue;
exports.getValue = getValue;
exports.exec = exec;
exports.make = make;
/* input Not a pure module */
