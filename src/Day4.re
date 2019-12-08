let (startRange, endRange) = (245318, 765747);
let size = 6;

let rec hasOnlyTwoAdjacentIdenticalDigits = (~next=?, ~digitCount=?, n: int) => {
  let currentDigit = n mod 10;
  switch (n, next, digitCount) {
  | (_, None, Some(digitCountMap))
  | (0, _, Some(digitCountMap)) =>
    digitCountMap |> List.exists(((_, count)) => count === 2)
  | (0, _, _) => false

  | (_, Some(num), None) when num === currentDigit =>
    let counts = [(currentDigit, 2)];
    hasOnlyTwoAdjacentIdenticalDigits(
      ~next=currentDigit,
      ~digitCount=counts,
      n / 10,
    );
  | (_, Some(num), Some(digitCountMap)) when num === currentDigit =>
    hasOnlyTwoAdjacentIdenticalDigits(
      ~next=currentDigit,
      ~digitCount=
        switch (digitCountMap) {
        | [(digit, count), ...rest] when num === currentDigit => [
            (digit, count + 1),
            ...rest,
          ]
        | _ => [(currentDigit, 1)]
        },
      n / 10,
    )

  | (_, Some(_), Some(digitCountMap)) =>
    let counts =
      switch (digitCountMap) {
      | [(digit, 2), ...rest] when digit === currentDigit => [
          (digit, 2),
          ...rest,
        ]
      | [(digit, _), ...rest] when digit === currentDigit => [
          (digit, 1),
          ...rest,
        ]
      | list => [(currentDigit, 1), ...list]
      };
    hasOnlyTwoAdjacentIdenticalDigits(
      ~next=currentDigit,
      ~digitCount=counts,
      n / 10,
    );

  | (_, Some(_), None)
  | (_, None, None) =>
    hasOnlyTwoAdjacentIdenticalDigits(
      ~next=currentDigit,
      ~digitCount=[(currentDigit, 1)],
      n / 10,
    )
  };
};

let rec hasTwoAdjacentIdenticalDigits = (~next=?, n: int) => {
  let currentDigit = n mod 10;
  switch (n, next) {
  | (0, _) => false
  | (_, Some(num)) when num === currentDigit => true
  | (_, Some(_))
  | (_, None) => hasTwoAdjacentIdenticalDigits(~next=currentDigit, n / 10)
  };
};

let rec areDigitsNeverDecreasing = (~next=?, n: int) => {
  let currentDigit = n mod 10;
  switch (n, next) {
  | (0, _) => true
  | (_, Some(num)) when num < currentDigit => false
  | (_, Some(_))
  | (_, None) => areDigitsNeverDecreasing(~next=currentDigit, n / 10)
  };
};

let matchesCriteria = (matcher, n: int) =>
  matcher(n) && areDigitsNeverDecreasing(n);

let rec findPasswords = (matcher, current: int, last: int, count: int) =>
  if (current >= last) {
    count;
  } else {
    let count = matchesCriteria(matcher, current) ? count + 1 : count;
    findPasswords(matcher, current + 1, last, count);
  };

let part1 =
  findPasswords(hasTwoAdjacentIdenticalDigits, startRange, endRange, 0);
Js.log(Printf.sprintf("Part 1: %d", part1));

let part2 =
  findPasswords(hasOnlyTwoAdjacentIdenticalDigits, startRange, endRange, 0);
Js.log(Printf.sprintf("Part 2: %d", part2));
