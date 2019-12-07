exception ParseException;
exception InvalidWireException;

let input =
  "R1009,D335,L942,D733,L398,U204,L521,D347,L720,U586,R708,D746,L292,U416,L824,U20,R359,D828,R716,U895,L498,D671,L325,D68,L667,U134,L435,D44,R801,U654,R188,U542,L785,D318,L806,U602,L465,U239,R21,U571,R653,U436,L52,U380,R446,D960,R598,U590,L47,U972,L565,D281,R790,U493,R864,D396,R652,D775,L939,D284,R554,U629,L842,D837,R554,D795,R880,D301,R948,U974,L10,D898,R588,D743,L334,U59,L413,U511,L132,U771,R628,D805,R465,D561,R18,D169,L580,D99,L508,U964,L870,D230,L472,U897,L85,U306,L103,U322,L637,U464,R129,D514,R454,U479,R801,U18,R929,U181,L113,D770,L173,D124,L122,U481,L666,D942,L534,U608,R90,U576,L641,U249,L857,U197,R783,D92,L938,D192,L698,D862,R995,U12,R766,D323,R934,U315,R956,D234,R983,D246,L153,U26,L779,D628,R174,D385,L758,D486,R132,U414,R915,D511,L152,D309,L708,D755,L679,D166,L699,U734,R55,D224,L582,U798,L348,U219,L304,U621,L788,D538,R781,D509,R486,U581,R759,D892,R16,D552,L82,D618,L309,D610,L645,U146,L328,U569,L307,D385,L249,D231,R928,U681,R384,D337,R715,D798,L788,D604,R517,U766,R368,U430,L49,U236,R621,U656,R997,U268,L18,D789,L935,D87,L670,U35,R463,D71,R268,U728,R693,D863,R656,D654,L350,U796,L72,U562,R56,U10,L651,D751,L557,D518,R901,D741,R787,D332,R723,D980,R206,U670,R645,D927,L641,D863,R478,D568,L858,D990,L124,D864,L162,U361,L407,U674,R508,D284,L675,D794,L138,U55,L781,U37,R956,D364,L111,U721,L91,U559,L852,U351,R994,U446,L162,D345,R92,D941,R572,U185,R615,D590,R459,D313,R127,D315,R96,U751,R210,D620,L790,U826,R410,D652,R549,D698,L805,U814,L364,U905,L96,U997,L689
L1008,D451,L146,D628,R877,U486,L464,U815,L119,U208,R686,U477,L510,D353,R189,D437,R461,D645,R639,U650,R491,D744,L798,U514,R598,U64,R668,U771,R21,U782,L564,U632,R23,U112,R947,U649,L205,D804,R277,U683,L828,U662,R890,U420,L908,U484,R535,D515,R390,U7,L287,D967,R497,U502,L893,D851,R426,D656,R622,U46,L106,U590,R646,D29,R467,D896,L155,U382,L992,D189,L34,U16,R132,U35,L586,U812,L539,D409,R776,D42,R58,U323,R569,D965,R648,D789,R478,D587,R162,D834,R979,D993,L944,U84,R93,U903,R491,U713,L646,U235,R120,U286,L919,U34,L662,U834,L812,D271,L73,U410,L758,U210,R712,U581,L520,D654,L981,D516,R312,U123,L153,U433,R368,U606,L882,U362,L261,U587,R441,D691,L699,U135,L825,D25,R142,U191,L358,D554,L487,D802,L542,D266,R283,U222,R113,D259,R828,U182,R402,U627,R769,D426,L768,U571,R118,U684,R803,D430,R942,U514,R711,D225,R299,U45,L214,U712,L673,U787,L164,D703,L616,D587,R624,D326,L614,D779,L904,D563,L98,U137,R687,U425,R615,U671,L361,D47,L767,D951,R791,D116,R664,U704,R291,U535,L322,D989,R467,U7,L974,D276,R901,U51,L567,D641,R112,U102,R753,D127,R486,D143,R259,U212,L97,U505,R377,U473,R514,D912,L928,U401,R772,D416,R695,U784,L524,D341,R402,U749,L1,U1,L109,U921,L754,U66,L927,U708,R551,D687,R129,D346,L408,D330,L300,D920,R170,D353,R97,D74,R850,D511,R275,U872,L748,U344,R610,D391,R963,D98,L89,U259,R651,U651,L31,D142,L104,U770,L482,D677,R823,D110,L606,U897,L631,U437,L551,D550,R301,D762,R349,D824,R260,U438,R249,D636,L386,U926,R367,U231,R752,U854,L481,D764,R516,D273,L726,D778,R483,U513,R129,D135,L224"
  |> Js.String.split("\n")
  |> Array.map(Js.String.split(","));

type wire =
  | Wire1
  | Wire2;

type minIntersection =
  | NotFound
  | Found((int, int), int);

type minSteps =
  | NotFound
  | Found(int);

type move = {
  wire,
  steps: array(int),
};

type state = {
  current: (int, int),
  stepCount: int,
  segments: Belt.Map.String.t(move),
  minIntersection,
  minSteps,
};

let wireCount = 2;

let intToWire = (wire: int) =>
  switch (wire) {
  | 1 => Wire1
  | 2 => Wire2
  | _ => raise(InvalidWireException)
  };

let getSteps = (move: move, wire: wire) => {
  switch (wire) {
  | Wire1 => move.steps[0]
  | Wire2 => move.steps[1]
  };
};
let setSteps = (move: move, wire: wire, value: int) => {
  switch (wire) {
  | Wire1 => move.steps[0] = value
  | Wire2 => move.steps[1] = value
  };
  move;
};

let distanceFromCenter = (x, y) => abs(x) + abs(y);

let key = (x, y) => string_of_int(x * 100000 + y);

let addDirection = ((px, py): (int, int), (dx, dy): (int, int)) => (
  px + dx,
  py + dy,
);

let determineIntersection =
    (state: state, (x, y) as position: (int, int), wire: wire) => {
  let state = {...state, stepCount: state.stepCount + 1};
  let mapKey = key(x, y);
  let setSegment = Belt.Map.String.set(state.segments, mapKey);

  let distance = distanceFromCenter(x, y);
  let sumSteps = (m: move) => getSteps(m, Wire1) + getSteps(m, Wire2);

  let updateMinSteps = (minSteps, steps) => {
    switch (minSteps) {
    | NotFound => Found(steps)
    | Found(stepCount) => Found(min(steps, stepCount))
    };
  };

  let updateMinIntersection = (move, minSteps) => {
    let steps = sumSteps(move);
    {
      ...state,
      current: position,
      segments: setSegment(move),
      minIntersection: Found(position, distance),
      minSteps: updateMinSteps(minSteps, steps),
    };
  };

  switch (
    Belt.Map.String.get(state.segments, mapKey),
    state.minIntersection,
    state.minSteps,
  ) {
  // same wire, don't do anything
  | (Some(move), NotFound, _)
  | (Some(move), Found(_, _), _) when move.wire === wire => {
      ...state,
      current: (x, y),
    }

  // intersection, distance < min distance
  | (Some(move), Found(_, minDistance), minSteps)
      when distance < minDistance =>
    let move = setSteps(move, wire, state.stepCount);
    updateMinIntersection(move, minSteps);

  // intersection, no min distance
  | (Some(move), NotFound, minSteps) =>
    let move = setSteps(move, wire, state.stepCount);
    updateMinIntersection(move, minSteps);

  // distance isn't < minDistance
  | (Some(move), Found(_, _), minSteps) =>
    let move = setSteps(move, wire, state.stepCount);
    let steps = sumSteps(move);
    {
      ...state,
      current: position,
      segments: setSegment(move),
      minSteps: updateMinSteps(minSteps, steps),
    };

  // we haven't previously seen this point yet
  | (None, _, _) =>
    let move =
      setSteps(
        {steps: Array.make(wireCount, 0), wire},
        wire,
        state.stepCount,
      );
    {...state, current: (x, y), segments: setSegment(move)};
  };
};

let rec updateMap =
        (
          {current} as state: state,
          direction: (int, int),
          amount: int,
          wire: wire,
        ) => {
  switch (amount, direction) {
  | (0, _) => state

  | (a, (x, y)) when a < 0 =>
    let s =
      determineIntersection(state, addDirection(current, (x, y)), wire);
    updateMap(s, direction, amount + 1, wire);

  | (a, (x, y)) when a > 0 =>
    let s =
      determineIntersection(state, addDirection(current, (x, y)), wire);
    updateMap(s, direction, amount - 1, wire);
  | _ => raise(ParseException)
  };
};

let directionToNumbers = (direction: string) => {
  switch (direction) {
  | "R" => (1, 0)
  | "L" => ((-1), 0)
  | "U" => (0, 1)
  | "D" => (0, (-1))
  | _ => raise(ParseException)
  };
};

let processMovement = (state: state, movement: string, wire: wire) => {
  switch (Js.Re.exec_(Js.Re.fromString("([RDLU])(\d+)"), movement)) {
  | Some(x) =>
    switch (Js.Re.captures(x) |> Array.map(Js.Nullable.toOption)) {
    | [|_, Some(direction), Some(distance)|] =>
      let state =
        updateMap(
          state,
          directionToNumbers(direction),
          int_of_string(distance),
          wire,
        );
      state;
    | _ => raise(ParseException)
    }
  | None => raise(ParseException)
  };
};

let processWires =
  switch (input) {
  | [|wire1, wire2|] =>
    let s = {
      segments: Belt.Map.String.empty,
      minIntersection: NotFound,
      current: (0, 0),
      stepCount: 0,
      minSteps: NotFound,
    };
    let s =
      wire1
      |> Array.fold_left(
           (state, move) => processMovement(state, move, Wire1),
           s,
         );
    let s =
      wire2
      |> Array.fold_left(
           (state, move) => processMovement(state, move, Wire2),
           {...s, current: (0, 0), stepCount: 0},
         );
    s;
  | _ => raise(ParseException)
  };

switch (processWires.minIntersection) {
| Found((x, y), distance) =>
  Js.log(Printf.sprintf("Part 1: %d at %d,%d", distance, x, y))
| _ => Js.log("Not found!")
};

switch (processWires.minSteps) {
| Found(steps) => Js.log(Printf.sprintf("Part 2: %d", steps))
| _ => Js.log("Not found!")
};
