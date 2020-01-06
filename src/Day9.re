exception UnknownInstruction(string);
exception InvalidMode(int);

let l2s = Int64.to_string;
let input =
  "1102,34463338,34463338,63,1007,63,34463338,63,1005,63,53,1101,3,0,1000,109,988,209,12,9,1000,209,6,209,3,203,0,1008,1000,1,63,1005,63,65,1008,1000,2,63,1005,63,902,1008,1000,0,63,1005,63,58,4,25,104,0,99,4,0,104,0,99,4,17,104,0,99,0,0,1101,26,0,1015,1101,29,0,1010,1102,1,24,1013,1102,1,33,1008,1102,36,1,1012,1101,0,572,1023,1101,35,0,1014,1101,0,38,1019,1102,1,30,1006,1101,0,890,1029,1101,34,0,1011,1101,28,0,1002,1102,1,1,1021,1101,0,37,1001,1101,0,197,1026,1101,22,0,1017,1102,1,895,1028,1101,0,20,1007,1102,21,1,1004,1102,1,39,1016,1101,0,0,1020,1102,1,190,1027,1101,0,775,1024,1102,31,1,1018,1101,0,23,1003,1101,0,25,1009,1101,770,0,1025,1101,0,27,1000,1102,1,575,1022,1101,0,32,1005,109,27,2106,0,0,1001,64,1,64,1106,0,199,4,187,1002,64,2,64,109,-18,21101,40,0,5,1008,1014,39,63,1005,63,219,1106,0,225,4,205,1001,64,1,64,1002,64,2,64,109,-6,1201,-1,0,63,1008,63,28,63,1005,63,251,4,231,1001,64,1,64,1105,1,251,1002,64,2,64,109,5,21102,41,1,3,1008,1011,38,63,1005,63,271,1105,1,277,4,257,1001,64,1,64,1002,64,2,64,109,-7,2102,1,1,63,1008,63,28,63,1005,63,299,4,283,1106,0,303,1001,64,1,64,1002,64,2,64,109,-7,1207,10,22,63,1005,63,321,4,309,1106,0,325,1001,64,1,64,1002,64,2,64,109,16,2107,31,-4,63,1005,63,345,1001,64,1,64,1105,1,347,4,331,1002,64,2,64,109,-9,1201,3,0,63,1008,63,18,63,1005,63,371,1001,64,1,64,1106,0,373,4,353,1002,64,2,64,109,7,1202,-7,1,63,1008,63,40,63,1005,63,393,1106,0,399,4,379,1001,64,1,64,1002,64,2,64,109,-5,1208,5,33,63,1005,63,417,4,405,1106,0,421,1001,64,1,64,1002,64,2,64,109,1,1202,2,1,63,1008,63,30,63,1005,63,443,4,427,1105,1,447,1001,64,1,64,1002,64,2,64,109,-7,2102,1,10,63,1008,63,19,63,1005,63,471,1001,64,1,64,1105,1,473,4,453,1002,64,2,64,109,6,2108,21,0,63,1005,63,489,1105,1,495,4,479,1001,64,1,64,1002,64,2,64,109,9,21108,42,42,0,1005,1012,513,4,501,1105,1,517,1001,64,1,64,1002,64,2,64,109,7,21107,43,44,-1,1005,1018,535,4,523,1106,0,539,1001,64,1,64,1002,64,2,64,109,-5,21101,44,0,2,1008,1016,44,63,1005,63,561,4,545,1105,1,565,1001,64,1,64,1002,64,2,64,2105,1,9,1106,0,581,4,569,1001,64,1,64,1002,64,2,64,109,13,21107,45,44,-9,1005,1018,597,1105,1,603,4,587,1001,64,1,64,1002,64,2,64,109,-25,2101,0,3,63,1008,63,32,63,1005,63,625,4,609,1105,1,629,1001,64,1,64,1002,64,2,64,109,7,1208,-7,30,63,1005,63,645,1105,1,651,4,635,1001,64,1,64,1002,64,2,64,109,-2,21102,46,1,9,1008,1016,46,63,1005,63,677,4,657,1001,64,1,64,1106,0,677,1002,64,2,64,109,-2,21108,47,48,9,1005,1014,697,1001,64,1,64,1105,1,699,4,683,1002,64,2,64,109,14,1205,2,713,4,705,1105,1,717,1001,64,1,64,1002,64,2,64,109,-7,1206,8,735,4,723,1001,64,1,64,1106,0,735,1002,64,2,64,109,-18,2101,0,6,63,1008,63,24,63,1005,63,759,1001,64,1,64,1106,0,761,4,741,1002,64,2,64,109,29,2105,1,1,4,767,1106,0,779,1001,64,1,64,1002,64,2,64,109,-5,1206,3,791,1106,0,797,4,785,1001,64,1,64,1002,64,2,64,109,-12,2107,31,-1,63,1005,63,819,4,803,1001,64,1,64,1105,1,819,1002,64,2,64,109,7,1205,7,835,1001,64,1,64,1105,1,837,4,825,1002,64,2,64,109,-11,1207,7,24,63,1005,63,853,1106,0,859,4,843,1001,64,1,64,1002,64,2,64,109,4,2108,27,-6,63,1005,63,881,4,865,1001,64,1,64,1106,0,881,1002,64,2,64,109,24,2106,0,-2,4,887,1106,0,899,1001,64,1,64,4,64,99,21102,27,1,1,21101,0,913,0,1106,0,920,21201,1,61934,1,204,1,99,109,3,1207,-2,3,63,1005,63,962,21201,-2,-1,1,21101,0,940,0,1106,0,920,21202,1,1,-1,21201,-2,-3,1,21101,0,955,0,1105,1,920,22201,1,-1,-2,1105,1,966,22102,1,-2,-2,109,-3,2105,1,0"
  |> Js.String.split(",")
  |> Array.map(Int64.of_string);

module Int64Map =
  Map.Make({
    type t = Int64.t;
    let compare = Int64.compare;
  });

type int64Map = Int64Map.t(int64);

type state = {
  mutable pc: int64,
  mutable relativeBase: int64,
  input: array(int64),
  mutable extraMemory: int64Map,
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

let (%!) = (a, b) => Int64.rem(a, b);
let (/!) = Int64.div;
let (+!) = Int64.add;
let ( *! ) = Int64.mul;

let getOpcode = (num: int64) => {
  let op = num %! 100L |> Int64.to_int;
  let mode1 = num /! 100L %! 10L |> Int64.to_int;
  let mode2 = num /! 1000L %! 10L |> Int64.to_int;
  let mode3 = num /! 10000L %! 10L |> Int64.to_int;
  /*
    Js.log(
      Printf.sprintf(
        "  OP %d [m1 %d] [m2 %d] [m3 %d] (%Ld)",
        op,
        mode1,
        mode2,
        mode3,
        num,
      ),
    );
   */

  (op, mode1, mode2, mode3);
};

let setValue = (state: state, addr: int64, value: int64) =>
  state.extraMemory = Int64Map.add(addr, value, state.extraMemory);

let readValue = (state: state, addr: int64) =>
  try (Int64Map.find(addr, state.extraMemory)) {
  | Not_found =>
    Js.log(Printf.sprintf("Memory not found at %Ld", addr));
    0L;
  };

let writeValue = (state: state, addr: int64, value: int64, mode: int): unit =>
  switch (mode) {
  | 2 =>
    let addr = addr +! state.relativeBase;
    setValue(state, addr, value);
  | _ => setValue(state, addr, value)
  };

let getValue =
    ({relativeBase} as state: state, addrOrValue: int64, mode: int) =>
  switch (mode) {
  | 0 => readValue(state, addrOrValue)
  | 1 => addrOrValue
  | 2 =>
    let addr = addrOrValue +! relativeBase;
    readValue(state, addr);

  | mode => raise(InvalidMode(mode))
  };

let rec exec = ({pc} as state: state): Js.Promise.t(state) => {
  open Js.Promise;

  let (opcode, mode1, mode2, mode3) = readValue(state, pc) |> getOpcode;
  switch (opcode) {
  | 1 =>
    let (inp1, inp2, output) = (
      readValue(state, pc +! 1L),
      readValue(state, pc +! 2L),
      readValue(state, pc +! 3L),
    );
    let result =
      getValue(state, inp1, mode1) +! getValue(state, inp2, mode2);
    writeValue(state, output, result, mode3);

    state.pc = state.pc +! 4L;
    exec(state);

  | 2 =>
    let (inp1, inp2, output) = (
      readValue(state, pc +! 1L),
      readValue(state, pc +! 2L),
      readValue(state, pc +! 3L),
    );
    let result =
      getValue(state, inp1, mode1) *! getValue(state, inp2, mode2);
    writeValue(state, output, result, mode3);

    state.pc = state.pc +! 4L;
    exec(state);

  | 3 =>
    let addr = readValue(state, pc +! 1L);
    Js.log("Input: ");

    ReadLine.createInterface(rl)
    |> then_(num => {
         writeValue(state, addr, Int64.of_string(num), mode1);
         state.pc = state.pc +! 2L;
         exec(state);
       });

  | 4 =>
    let addr = readValue(state, pc +! 1L);
    let output = getValue(state, addr, mode1);
    Js.log2("Out: ", output |> l2s);
    state.pc = state.pc +! 2L;
    exec(state);

  | 5 =>
    let (condition, addr) = (
      readValue(state, pc +! 1L),
      readValue(state, pc +! 2L),
    );
    let cmpValue = getValue(state, condition, mode1);
    if (cmpValue != 0L) {
      state.pc = getValue(state, addr, mode2);
    } else {
      state.pc = state.pc +! 3L;
    };
    exec(state);

  | 6 =>
    let (condition, addr) = (
      readValue(state, pc +! 1L),
      readValue(state, pc +! 2L),
    );

    let cmpValue = getValue(state, condition, mode1);
    if (cmpValue == 0L) {
      state.pc = getValue(state, addr, mode2);
    } else {
      state.pc = state.pc +! 3L;
    };
    exec(state);

  | 7 =>
    let (inp1, inp2, addr) = (
      readValue(state, pc +! 1L),
      readValue(state, pc +! 2L),
      readValue(state, pc +! 3L),
    );
    let v1 = getValue(state, inp1, mode1);
    let v2 = getValue(state, inp2, mode2);

    if (v1 < v2) {
      writeValue(state, addr, 1L, mode3);
    } else {
      writeValue(state, addr, 0L, mode3);
    };
    state.pc = state.pc +! 4L;
    exec(state);

  | 8 =>
    let (inp1, inp2, addr) = (
      readValue(state, pc +! 1L),
      readValue(state, pc +! 2L),
      readValue(state, pc +! 3L),
    );
    let v1 = getValue(state, inp1, mode1);
    let v2 = getValue(state, inp2, mode2);

    if (v1 == v2) {
      writeValue(state, addr, 1L, mode3);
    } else {
      writeValue(state, addr, 0L, mode3);
    };
    state.pc = state.pc +! 4L;
    exec(state);

  | 9 =>
    let param = readValue(state, pc +! 1L);

    let relativeBase = getValue(state, param, mode1);
    state.relativeBase = state.relativeBase +! relativeBase;
    Js.log(
      Printf.sprintf("Updating relative base to %Ld", state.relativeBase),
    );
    state.pc = state.pc +! 2L;
    exec(state);

  | 99 => resolve(state)

  | op =>
    raise(UnknownInstruction(Printf.sprintf("invalid instruction %d", op)))
  };
};

let make = (input: array(int64)): state => {
  let (memory, _) =
    input
    |> Array.fold_left(
         ((memMap, index), item) => (
           Int64Map.add(index, item, memMap),
           index +! 1L,
         ),
         (Int64Map.empty, 0L),
       );

  {pc: 0L, relativeBase: 0L, input, extraMemory: memory};
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
