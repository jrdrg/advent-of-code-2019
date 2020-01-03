exception UnknownInstruction(string);
exception InvalidMode(int);

let input =
  //   "3,225,1,225,6,6,1100,1,238,225,104,0,1102,67,92,225,1101,14,84,225,1002,217,69,224,101,-5175,224,224,4,224,102,8,223,223,101,2,224,224,1,224,223,223,1,214,95,224,101,-127,224,224,4,224,102,8,223,223,101,3,224,224,1,223,224,223,1101,8,41,225,2,17,91,224,1001,224,-518,224,4,224,1002,223,8,223,101,2,224,224,1,223,224,223,1101,37,27,225,1101,61,11,225,101,44,66,224,101,-85,224,224,4,224,1002,223,8,223,101,6,224,224,1,224,223,223,1102,7,32,224,101,-224,224,224,4,224,102,8,223,223,1001,224,6,224,1,224,223,223,1001,14,82,224,101,-174,224,224,4,224,102,8,223,223,101,7,224,224,1,223,224,223,102,65,210,224,101,-5525,224,224,4,224,102,8,223,223,101,3,224,224,1,224,223,223,1101,81,9,224,101,-90,224,224,4,224,102,8,223,223,1001,224,3,224,1,224,223,223,1101,71,85,225,1102,61,66,225,1102,75,53,225,4,223,99,0,0,0,677,0,0,0,0,0,0,0,0,0,0,0,1105,0,99999,1105,227,247,1105,1,99999,1005,227,99999,1005,0,256,1105,1,99999,1106,227,99999,1106,0,265,1105,1,99999,1006,0,99999,1006,227,274,1105,1,99999,1105,1,280,1105,1,99999,1,225,225,225,1101,294,0,0,105,1,0,1105,1,99999,1106,0,300,1105,1,99999,1,225,225,225,1101,314,0,0,106,0,0,1105,1,99999,8,226,226,224,102,2,223,223,1005,224,329,1001,223,1,223,1108,677,677,224,1002,223,2,223,1006,224,344,101,1,223,223,1007,226,677,224,102,2,223,223,1005,224,359,101,1,223,223,1007,677,677,224,1002,223,2,223,1006,224,374,101,1,223,223,1108,677,226,224,1002,223,2,223,1005,224,389,1001,223,1,223,108,226,677,224,102,2,223,223,1006,224,404,101,1,223,223,1108,226,677,224,102,2,223,223,1005,224,419,101,1,223,223,1008,677,677,224,102,2,223,223,1005,224,434,101,1,223,223,7,677,226,224,1002,223,2,223,1005,224,449,101,1,223,223,1008,226,226,224,102,2,223,223,1005,224,464,1001,223,1,223,107,226,677,224,1002,223,2,223,1006,224,479,1001,223,1,223,107,677,677,224,102,2,223,223,1005,224,494,1001,223,1,223,1008,226,677,224,102,2,223,223,1006,224,509,1001,223,1,223,1107,677,226,224,102,2,223,223,1005,224,524,101,1,223,223,1007,226,226,224,1002,223,2,223,1006,224,539,1001,223,1,223,107,226,226,224,102,2,223,223,1006,224,554,101,1,223,223,108,677,677,224,1002,223,2,223,1006,224,569,1001,223,1,223,7,226,677,224,102,2,223,223,1006,224,584,1001,223,1,223,8,677,226,224,102,2,223,223,1005,224,599,101,1,223,223,1107,677,677,224,1002,223,2,223,1005,224,614,101,1,223,223,8,226,677,224,102,2,223,223,1005,224,629,1001,223,1,223,7,226,226,224,1002,223,2,223,1006,224,644,1001,223,1,223,108,226,226,224,1002,223,2,223,1006,224,659,101,1,223,223,1107,226,677,224,1002,223,2,223,1006,224,674,101,1,223,223,4,223,99,226"
  //   "109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99"
  "104,1125899906842624,99"
  |> Js.String.split(",")
  |> Array.map(Int64.of_string);

let l = 32L;

type state = {
  mutable pc: int,
  mutable relativeBase: int64,
  input: array(int64),
  extraMemory: Belt.Map.Int.t(int64),
};

module ReadLine = {
  type t;
  type interface;

  [@bs.module] external make: t = "readline";

  let createInterface: t => Js.Promise.t(string) = [%bs.raw
    {|
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
    |}
  ];
};

let rl = ReadLine.make;

let getOpcode = num => {
  let op = num mod 100;
  let mode1 = num / 100 mod 10;
  let mode2 = num / 1000 mod 10;
  let mode3 = num / 10000 mod 10;
  Js.log(
    Printf.sprintf(
      "  OP %d m1 %d m2 %d m3 %d (%d)",
      op,
      mode1,
      mode2,
      mode3,
      num,
    ),
  );

  (op, mode1, mode2, mode3);
};

let writeValue = (state: state, addr: int64, value: int64) =>
  if (addr >= Int64.of_int(Array.length(state.input))) {
    Js.log("OUTOFBOUNDS");
    Belt.Map.Int.set(state.extraMemory, addr, value) |> ignore;
  } else {
    state.input[addr] = value;
  };

let readValue = (state: state, addr: int) =>
  if (addr >= Array.length(state.input)) {
    Js.log("OUTOFBOUNDS");
    state.extraMemory->Belt.Map.Int.get(addr)->Belt.Option.getWithDefault(0);
  } else {
    state.input[addr];
  };

let getValue = ({relativeBase} as state: state, addrOrValue: int, mode: int) => {
  switch (mode) {
  | 0 =>
    Js.log3("   Address", addrOrValue, readValue(state, addrOrValue));
    readValue(state, addrOrValue);
  | 1 =>
    Js.log2("   Immediate mode", addrOrValue);
    addrOrValue;
  | 2 =>
    let addr = addrOrValue + relativeBase;
    Js.log4("   Relative mode", addrOrValue, relativeBase, addr);
    readValue(state, addr);

  | mode => raise(InvalidMode(mode))
  };
};

let rec exec = ({input} as state: state): Js.Promise.t(state) => {
  open Js.Promise;

  let {pc} = state;
  let (opcode, mode1, mode2, _mode3) = getOpcode(input[pc]);
  switch (opcode) {
  | 1 =>
    let (inp1, inp2, output) = (input[pc + 1], input[pc + 2], input[pc + 3]);
    let result = getValue(state, inp1, mode1) + getValue(state, inp2, mode2);
    // input[output] = result;
    writeValue(state, output, result);
    Js.log(Printf.sprintf("   :: Storing %d at address %d", result, output));
    state.pc = state.pc + 4;
    exec(state);

  | 2 =>
    let (inp1, inp2, output) = (input[pc + 1], input[pc + 2], input[pc + 3]);
    let result = getValue(state, inp1, mode1) * getValue(state, inp2, mode2);
    // input[output] = result;
    writeValue(state, output, result);
    Js.log(Printf.sprintf("   :: Storing %d at address %d", result, output));
    state.pc = state.pc + 4;
    exec(state);

  | 3 =>
    Js.log("Input: ");
    let addr = input[pc + 1];
    ReadLine.createInterface(rl)
    |> then_(num => {
         //  input[addr] = int_of_string(num);
         writeValue(state, addr, int_of_string(num));
         state.pc = state.pc + 2;
         exec(state);
       });

  | 4 =>
    let addr = input[pc + 1];
    let output = getValue(state, addr, mode1);
    Js.log2("Out: ", output);
    state.pc = state.pc + 2;
    exec(state);

  | 5 =>
    let (condition, addr) = (input[pc + 1], input[pc + 2]);
    if (getValue(state, condition, mode1) !== 0) {
      state.pc = getValue(state, addr, mode2);
    } else {
      state.pc = state.pc + 3;
    };
    exec(state);

  | 6 =>
    let (condition, addr) = (input[pc + 1], input[pc + 2]);
    if (getValue(state, condition, mode1) === 0) {
      state.pc = getValue(state, addr, mode2);
    } else {
      state.pc = state.pc + 3;
    };
    exec(state);

  | 7 =>
    let (inp1, inp2, addr) = (input[pc + 1], input[pc + 2], input[pc + 3]);
    if (getValue(state, inp1, mode1) < getValue(state, inp2, mode2)) {
      //   input[addr] = 1;
      writeValue(state, addr, 1);
    } else {
      //   input[addr] = 0;
      writeValue(state, addr, 0);
    };
    state.pc = state.pc + 4;
    exec(state);

  | 8 =>
    let (inp1, inp2, addr) = (input[pc + 1], input[pc + 2], input[pc + 3]);
    if (getValue(state, inp1, mode1) === getValue(state, inp2, mode2)) {
      //   input[addr] = 1;
      writeValue(state, addr, 1);
    } else {
      //   input[addr] = 0;
      writeValue(state, addr, 0);
    };
    state.pc = state.pc + 4;
    exec(state);

  | 9 =>
    let relativeBase = input[pc + 1];
    state.relativeBase = state.relativeBase + relativeBase;
    Js.log2("Updating relative base to ", state.relativeBase);
    state.pc = state.pc + 2;
    exec(state);

  | 99 => resolve(state)
  | op =>
    raise(UnknownInstruction(Printf.sprintf("invalid instruction %d", op)))
  };
};

let make = (input: array(int)): state => {
  pc: 0,
  relativeBase: 0,
  input,
  extraMemory: Belt.Map.Int.empty,
};

exec(make(input))
|> Js.Promise.then_(_ => {
     Js.log("Done");
     Js.Promise.resolve();
   })
|> Js.Promise.catch(err => {
     Js.log(err);
     Js.Promise.resolve();
   });