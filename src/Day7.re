exception UnknownInstruction(string);
exception InvalidMode(int);
exception NoOutput;

let input =
  "3,8,1001,8,10,8,105,1,0,0,21,38,55,72,93,118,199,280,361,442,99999,3,9,1001,9,2,9,1002,9,5,9,101,4,9,9,4,9,99,3,9,1002,9,3,9,1001,9,5,9,1002,9,4,9,4,9,99,3,9,101,4,9,9,1002,9,3,9,1001,9,4,9,4,9,99,3,9,1002,9,4,9,1001,9,4,9,102,5,9,9,1001,9,4,9,4,9,99,3,9,101,3,9,9,1002,9,3,9,1001,9,3,9,102,5,9,9,101,4,9,9,4,9,99,3,9,101,1,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,99,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,101,1,9,9,4,9,99,3,9,101,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,99,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,99,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,99"
  |> Js.String.split(",")
  |> Array.map(String.trim)
  |> Array.map(int_of_string);

type state = {
  mutable pc: int,
  mutable output: option(int),
  input: array(int),
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
  (op, mode1, mode2, mode3);
};

let getValue = (input: array(int), addrOrValue: int, mode: int) => {
  switch (mode) {
  | 0 => input[addrOrValue]
  | 1 => addrOrValue
  | mode => raise(InvalidMode(mode))
  };
};

type inputProvider = unit => Js.Promise.t(string);

let rec exec =
        (getInput: inputProvider, {input} as state: state)
        : Js.Promise.t(state) => {
  open Js.Promise;

  let {pc} = state;
  let (opcode, mode1, mode2, _mode3) = getOpcode(input[pc]);
  switch (opcode) {
  | 1 =>
    let (inp1, inp2, output) = (input[pc + 1], input[pc + 2], input[pc + 3]);
    let result = getValue(input, inp1, mode1) + getValue(input, inp2, mode2);
    input[output] = result;
    state.pc = state.pc + 4;
    exec(getInput, state);

  | 2 =>
    let (inp1, inp2, output) = (input[pc + 1], input[pc + 2], input[pc + 3]);
    let result = getValue(input, inp1, mode1) * getValue(input, inp2, mode2);
    input[output] = result;
    state.pc = state.pc + 4;
    exec(getInput, state);

  | 3 =>
    let addr = input[pc + 1];
    getInput()
    |> then_(num => {
         input[addr] = int_of_string(num);
         state.pc = state.pc + 2;
         exec(getInput, state);
       });

  | 4 =>
    let addr = input[pc + 1];
    let output = getValue(input, addr, mode1);
    state.pc = state.pc + 2;
    state.output = Some(output);
    exec(getInput, state);

  | 5 =>
    let (condition, addr) = (input[pc + 1], input[pc + 2]);
    if (getValue(input, condition, mode1) !== 0) {
      state.pc = getValue(input, addr, mode2);
    } else {
      state.pc = state.pc + 3;
    };
    exec(getInput, state);

  | 6 =>
    let (condition, addr) = (input[pc + 1], input[pc + 2]);
    if (getValue(input, condition, mode1) === 0) {
      state.pc = getValue(input, addr, mode2);
    } else {
      state.pc = state.pc + 3;
    };
    exec(getInput, state);

  | 7 =>
    let (inp1, inp2, addr) = (input[pc + 1], input[pc + 2], input[pc + 3]);
    if (getValue(input, inp1, mode1) < getValue(input, inp2, mode2)) {
      input[addr] = 1;
    } else {
      input[addr] = 0;
    };
    state.pc = state.pc + 4;
    exec(getInput, state);

  | 8 =>
    let (inp1, inp2, addr) = (input[pc + 1], input[pc + 2], input[pc + 3]);
    if (getValue(input, inp1, mode1) === getValue(input, inp2, mode2)) {
      input[addr] = 1;
    } else {
      input[addr] = 0;
    };
    state.pc = state.pc + 4;
    exec(getInput, state);

  | 99 => resolve(state)
  | op =>
    raise(UnknownInstruction(Printf.sprintf("invalid instruction %d", op)))
  };
};

let init = i => {pc: 0, input: i, output: None};

let getInput = () => {
  ReadLine.createInterface(rl);
};

// let input =
//   "3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,
// 1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0"
//   |> Js.String.split(",")
//   |> Array.map(String.trim)
//   |> Array.map(int_of_string);

let inputState = [1, 0, 4, 3, 2];
Js.log(inputState |> List.length);

let getInputFromList = (inputs: list(int), lastOutput: int) => {
  let readInput = ref(false);
  () => {
    readInput^
      ? {
        Js.Promise.resolve(string_of_int(lastOutput));
      }
      : {
        let inp = List.hd(inputs);
        readInput := true;
        Js.Promise.resolve(string_of_int(inp));
      };
  };
};

let rec run = (~output=0, instructions, inputs) => {
  switch (inputs) {
  | [_, ...rest] =>
    exec(getInputFromList(inputs, output), init(instructions))
    |> Js.Promise.then_(result =>
         switch (result.output) {
         | Some(out) => run(~output=out, instructions, rest)
         | None => raise(NoOutput)
         }
       )
  | [] => Js.Promise.resolve(output)
  };
};

let makePhaseCombinations = () => {
  let nums = [0, 1, 2, 3, 4];
  // this is pretty ugly
  let phases =
    nums
    |> List.map(i1 => {
         let num2 = nums |> List.filter(i => i !== i1);
         num2
         |> List.map(i2 => {
              let num3 = num2 |> List.filter(i => i !== i1 && i !== i2);
              num3
              |> List.map(i3 => {
                   let num4 =
                     num3 |> List.filter(i => i !== i1 && i !== i2 && i !== i3);
                   num4
                   |> List.map(i4 => {
                        let num5 =
                          num4
                          |> List.filter(i =>
                               i !== i1 && i !== i2 && i !== i3 && i !== i4
                             );
                        num5 |> List.map(i5 => [i1, i2, i3, i4, i5]);
                      });
                 });
            });
       })
    |> List.concat
    |> List.concat
    |> List.concat
    |> List.concat;

  phases;
};

let rec tryCombinations = (~highest=0, phaseCombinations) => {
  switch (phaseCombinations) {
  | [phases, ...rest] =>
    run(input, phases)
    |> Js.Promise.then_(output => {
         let h = max(output, highest);
         if (output > highest) {
           Js.log(Printf.sprintf("Prev: %d, new: %d", highest, output));
         };
         tryCombinations(~highest=h, rest);
       })
  | [] => Js.Promise.resolve(highest)
  };
};

let phases = makePhaseCombinations();
Js.log2("Combinations", List.length(phases));

tryCombinations(phases)
|> Js.Promise.then_(highest => {
     Js.log2("Part 1", highest);
     Js.Promise.resolve();
   });
