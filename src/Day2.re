exception UnknownInstruction(int);

let input =
  "1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,13,1,19,1,19,10,23,2,10,23,27,1,27,6,31,1,13,31,35,1,13,35,39,1,39,10,43,2,43,13,47,1,47,9,51,2,51,13,55,1,5,55,59,2,59,9,63,1,13,63,67,2,13,67,71,1,71,5,75,2,75,13,79,1,79,6,83,1,83,5,87,2,87,6,91,1,5,91,95,1,95,13,99,2,99,6,103,1,5,103,107,1,107,9,111,2,6,111,115,1,5,115,119,1,119,2,123,1,6,123,0,99,2,14,0,0"
  |> Js.String.split(",")
  |> Array.map(String.trim)
  |> Array.map(int_of_string);

type state = {
  mutable pc: int,
  input: array(int),
};

let rec exec = ({input} as state: state): state => {
  let {pc} = state;
  let opcode = input[pc];
  switch (opcode) {
  | 1 =>
    let (inp1, inp2, output) = (input[pc + 1], input[pc + 2], input[pc + 3]);
    let result = input[inp1] + input[inp2];
    input[output] = result;
    state.pc = state.pc + 4;
    exec(state);
  | 2 =>
    let (inp1, inp2, output) = (input[pc + 1], input[pc + 2], input[pc + 3]);
    let result = input[inp1] * input[inp2];
    input[output] = result;
    state.pc = state.pc + 4;
    exec(state);
  | 99 => state
  | op => raise(UnknownInstruction(op))
  };
};

let changeNounAndVerb = (noun: int, verb: int) => {
  let cloned = Array.copy(input);
  cloned[1] = noun;
  cloned[2] = verb;
  cloned;
};

let getOutput = (input: array(int)) => input[0];

// restore the gravity assist program!
let input = changeNounAndVerb(12, 2);

let result = exec({pc: 0, input});

Js.log(Printf.sprintf("Part 1: %d", getOutput(input)));

let magicOutput = 19690720;

let puzzleAnswer = (noun: int, verb: int) => 100 * noun + verb;

type answer =
  | Found(int)
  | NotFound;

let params = Array.init(100 * 100, idx => (idx / 100, idx mod 100));

let rec findAnswer = index => {
  let (noun, verb) = params[index];
  let input = changeNounAndVerb(noun, verb);
  let output = exec({pc: 0, input}).input |> getOutput;
  if (output == magicOutput) {
    Found(puzzleAnswer(noun, verb));
  } else {
    findAnswer(index + 1);
  };
};

Js.log("Part 2");
switch (findAnswer(0)) {
| Found(a) => Js.log2("Found", a)
| NotFound => Js.log("Not found")
};
