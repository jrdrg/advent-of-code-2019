type vec3 = {
  x: int,
  y: int,
  z: int,
};

type position =
  | Position(vec3);
type velocity =
  | Velocity(vec3);

type moon = {
  mutable position,
  mutable velocity,
};

exception InvalidInput(string);

let toString = moon => {
  let Position(position) = moon.position;
  let Velocity(velocity) = moon.velocity;

  Js.log(
    Printf.sprintf(
      "P=<x:%d,y:%d,z:%d>,V=<x:%d,y:%d,z:%d>",
      position.x,
      position.y,
      position.z,
      velocity.x,
      velocity.y,
      velocity.z,
    ),
  );
};

let strToVec = (str: string): vec3 => {
  let reg = Js.Re.fromString("<x=([\\-\\d]+), y=([\\-\\d]+), z=([\\-\\d]+)>");
  let result = Js.Re.exec_(reg, str);
  let matches =
    switch (result) {
    | Some(matches) =>
      matches
      |> Js.Re.captures
      |> Array.map(c =>
           Js.Nullable.toOption(c) |> Js.Option.getWithDefault("0")
         )
    | None => raise(InvalidInput(str))
    };
  {
    x: matches[1] |> int_of_string,
    y: matches[2] |> int_of_string,
    z: matches[3] |> int_of_string,
  };
};

let prepareInput = (str: string) =>
  str |> Js.String.split("\n") |> Array.map(String.trim);

let makeMoon = (str: string) => {
  let pos = strToVec(str);
  {position: Position(pos), velocity: Velocity({x: 0, y: 0, z: 0})};
};

let initMoons = (inputs: array(string)) => {
  inputs |> Array.map(makeMoon);
};

let input =
  "<x=-1, y=7, z=3>
<x=12, y=2, z=-13>
<x=14, y=18, z=-8>
<x=17, y=4, z=-4>"
  |> prepareInput;

let moons = input |> Array.map(makeMoon);

// Js.log(moons);

let determineGravityAmount = (amount1: int, amount2: int) => {
  switch (amount1, amount2) {
  | (a1, a2) when a1 === a2 => 0
  | (a1, a2) when a1 > a2 => (-1)
  | (a1, a2) when a1 < a2 => 1
  | _ => raise(InvalidInput("determineGravityAmount"))
  };
};

let updateVelocity = (Velocity(v1), Velocity(v2)) => {
  Velocity({x: v1.x + v2.x, y: v1.y + v2.y, z: v1.z + v2.z});
};

let applyGravity =
    (Position(moon1): position, Position(moon2): position): velocity => {
  Velocity({
    x: determineGravityAmount(moon1.x, moon2.x),
    y: determineGravityAmount(moon1.y, moon2.y),
    z: determineGravityAmount(moon1.z, moon2.z),
  });
};

let applyVelocity =
    (
      {
        position: Position({x: px, y: py, z: pz}),
        velocity: Velocity({x: vx, y: vy, z: vz}),
      } as moon,
    )
    : moon => {
  {...moon, position: Position({x: px + vx, y: py + vy, z: pz + vz})};
};

// let moons =
//   "<x=-1, y=0, z=2>
// <x=2, y=-10, z=-7>
// <x=4, y=-8, z=8>
// <x=3, y=5, z=-1>"
//   |> prepareInput
//   |> initMoons;

// let moons =
//   "<x=-8, y=-10, z=0>
// <x=5, y=5, z=10>
// <x=2, y=-7, z=3>
// <x=9, y=-8, z=-3>"
//   |> prepareInput
//   |> initMoons;

let determinePairs = (moons: array(moon)): array((moon, moon)) => {
  //   Js.log("DETERMINE PAIRS");
  let moonList =
    moons |> Array.to_list |> List.mapi((index, moon) => (moon, index));

  moonList
  |> List.map(((m, i1)) =>
       moonList
       |> List.filter(((_, i2)) => i1 !== i2)
       |> List.map(((m2, _)) => (m, m2))
     )
  |> List.concat
  |> Array.of_list;
};

let timeStep = (moons: array(moon)) => {
  let pairs = determinePairs(moons);

  pairs
  |> Array.iter(((moon1, moon2)) => {
       let m1vel = applyGravity(moon1.position, moon2.position);
       moon1.velocity = updateVelocity(m1vel, moon1.velocity);
     });

  moons
  |> Array.iter(moon => {
       let m = applyVelocity(moon);
       moon.position = m.position;
     });

  moons;
};

let simulate = (moons, steps) => {
  Belt.Range.forEach(0, steps - 1, _ => timeStep(moons) |> ignore);
  moons;
};

let potentialEnergy = ({position: Position({x, y, z})}) => {
  abs(x) + abs(y) + abs(z);
};

let kineticEnergy = ({velocity: Velocity({x, y, z})}) => {
  abs(x) + abs(y) + abs(z);
};

let totalEnergy = (moons: array(moon)) => {
  moons
  |> Array.fold_left(
       (sum, moon) => {
         let total = potentialEnergy(moon) * kineticEnergy(moon);
         sum + total;
       },
       0,
     );
};

simulate(moons, 1000);

Js.log2("Part 1: ", totalEnergy(moons));