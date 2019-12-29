exception InvalidLine;
exception ChemicalAlreadyExists;
exception CannotFindReactionFor(string);

type chemical = {
  name: string,
  quantity: int,
};

type reaction = {
  inputs: list(chemical),
  output: chemical,
};

type createdBy = {
  outputQuantity: int,
  inputs: list(chemical),
  leftoverOre: int,
};

// 10 ore => 10 A
// "A" => { 10, [{"ORE", 10}] }
// 7 A, 1 B => 1 C
// "C" => { 1, [{"A", 7}, {"B", 1}] }
type chemicalMap = Belt.Map.String.t(createdBy);

let addToMap = ({inputs, output}: reaction, map: chemicalMap): chemicalMap => {
  let {name, quantity} = output;
  let created: createdBy = {outputQuantity: quantity, inputs, leftoverOre: 0};
  Belt.Map.String.update(map, name, v =>
    switch (v) {
    | None => Some(created)
    | Some(_) => raise(ChemicalAlreadyExists)
    }
  );
};

let parseChemical = (str: string) => {
  let asArray = str |> Js.String.split(" ") |> Array.map(String.trim);
  let (quantity, name) = (asArray[0], asArray[1]);
  {name, quantity: int_of_string(quantity)};
};

let parseLine = (line): reaction => {
  let parts = line |> Js.String.split("=>") |> Array.map(String.trim);
  let input = parts[0];
  let output = parts[1];
  Js.log3(input, "::", output);
  let inputs =
    input
    |> Js.String.split(",")
    |> Array.map(String.trim)
    |> Array.map(parseChemical)
    |> Array.to_list;

  {inputs, output: parseChemical(output)};
};

let makeInput = (str: string) =>
  str
  |> Js.String.split("\n")
  |> Array.map(String.trim)
  |> Array.map(parseLine);

let input = "10 ORE => 10 A
1 ORE => 1 B
7 A, 1 B => 1 C
7 A, 1 C => 1 D
7 A, 1 D => 1 E
7 A, 1 E => 1 FUEL";

let input = "9 ORE => 2 A
8 ORE => 3 B
7 ORE => 5 C
3 A, 4 B => 1 AB
5 B, 7 C => 1 BC
4 C, 1 A => 1 CA
2 AB, 3 BC, 4 CA => 1 FUEL";

let input = "157 ORE => 5 NZVS
165 ORE => 6 DCFZ
44 XJWVT, 5 KHKGT, 1 QDVJ, 29 NZVS, 9 GPVTF, 48 HKGWZ => 1 FUEL
12 HKGWZ, 1 GPVTF, 8 PSHF => 9 QDVJ
179 ORE => 7 PSHF
177 ORE => 5 HKGWZ
7 DCFZ, 7 PSHF => 2 XJWVT
165 ORE => 2 GPVTF
3 DCFZ, 7 NZVS, 5 HKGWZ, 10 PSHF => 8 KHKGT";

let input = "180 ORE => 9 DQFL
3 HGCR, 9 TKRT => 8 ZBLC
1 MZQLG, 12 RPLCK, 8 PDTP => 8 VCFX
3 ZBLC, 19 VFZX => 1 SJQL
1 CRPGK => 4 TPRT
7 HGCR, 4 TGCW, 1 VFZX => 9 JBPHS
8 GJHX => 4 NSDBV
1 VFTG => 2 QNWD
1 WDKW, 2 DWRH, 6 VNMV, 2 HFHL, 55 GJHX, 4 NSDBV, 15 KLJMS, 17 KZDJ => 1 FUEL
2 JHSJ, 15 JNWJ, 1 ZMFXQ => 4 GVRK
1 PJFBD => 3 MZQLG
1 SJQL, 11 LPVWN => 9 DLZS
3 PRMJ, 2 XNWV => 6 JHSJ
4 SJQL => 8 PJFBD
14 QNWD => 6 STHQ
5 CNLFV, 2 VFTG => 9 XNWV
17 LWNKB, 6 KBWF, 3 PLSCB => 8 KZDJ
6 LHWZQ, 5 LWNKB => 3 ZDWX
5 RPLCK, 2 LPVWN => 8 ZMFXQ
1 QNWD, 2 TKRT => 3 CRPGK
1 JBPHS, 1 XNWV => 6 TLRST
21 ZDWX, 3 FZDP, 4 CRPGK => 6 PDTP
1 JCVP => 1 WXDVT
2 CRPGK => 9 FGVL
4 DQFL, 2 VNMV => 1 HGCR
2 GVRK, 2 VCFX, 3 PJFBD, 1 PLSCB, 23 FZDP, 22 PCSM, 1 JLVQ => 6 HFHL
1 CRPGK, 5 PJFBD, 4 XTCP => 8 PLSCB
1 HTZW, 17 FGVL => 3 LHWZQ
2 KBWF => 4 DQKLC
2 LHWZQ => 2 PRMJ
2 DLZS, 2 VCFX, 15 PDTP, 14 ZDWX, 35 NBZC, 20 JVMF, 1 BGWMS => 3 DWRH
2 TKVCX, 6 RPLCK, 2 HTZW => 4 XTCP
8 CNLFV, 1 NRSD, 1 VFTG => 9 VFZX
1 TLRST => 4 WDKW
9 VFCZG => 7 GJHX
4 FZDP => 8 JLVQ
2 ZMFXQ, 2 STHQ => 6 QDZB
2 SJQL, 8 ZDWX, 6 LPRL, 6 WXDVT, 1 TPRT, 1 JNWJ => 8 KLJMS
6 JBPHS, 2 ZBLC => 6 HTZW
1 PDTP, 2 LHWZQ => 8 JNWJ
8 ZBLC => 7 TKVCX
2 WDKW, 31 QDZB => 4 PCSM
15 GJHX, 5 TKVCX => 7 FZDP
15 SJQL, 3 PRMJ => 4 JCVP
31 CNLFV => 1 TGCW
1 TLRST, 2 WDKW => 9 KBWF
102 ORE => 7 VNMV
103 ORE => 5 CNLFV
163 ORE => 2 VFTG
5 NRSD, 1 STHQ => 3 VFCZG
16 LPVWN, 13 KBWF => 2 BGWMS
5 BGWMS, 11 SJQL, 9 FZDP => 6 NBZC
175 ORE => 7 NRSD
5 HTZW => 4 LPVWN
4 PRMJ => 7 JVMF
6 PCSM, 8 DQKLC => 7 LPRL
2 CNLFV => 7 TKRT
3 FZDP => 3 LWNKB
1 HTZW => 4 RPLCK";

let inp = makeInput(input);

let map =
  inp
  |> Array.fold_left(
       (m, reaction) => addToMap(reaction, m),
       Belt.Map.String.empty,
     );

module Reaction = {
  type leftovers = Belt.Map.String.t(int);

  let getReaction = (chemical: chemical, reactions: chemicalMap) => {
    Belt.Map.String.get(reactions, chemical.name);
  };

  let getLeftovers = (chemical: chemical, leftovers: leftovers) => {
    Belt.Map.String.get(leftovers, chemical.name);
  };

  let updateLeftovers =
      (leftovers: leftovers, name: string, leftover: int => int) => {
    Belt.Map.String.update(leftovers, name, c =>
      switch (c) {
      | None => Some(leftover(0))
      | Some(l) => Some(leftover(l))
      }
    );
  };

  let rec getOreFor =
          (
            ~leftovers: ref(leftovers)=ref(Belt.Map.String.empty),
            chemical: chemical,
            reactions: chemicalMap,
          ) => {
    let createdBy = getReaction(chemical, reactions);
    let leftoverAmount = getLeftovers(chemical, leftovers^);

    Printf.printf("Creating %d %s\n", chemical.quantity, chemical.name);

    switch (chemical.name, createdBy, leftoverAmount) {
    | ("ORE", _, _) => chemical.quantity
    | (_, None, _) => raise(CannotFindReactionFor(chemical.name))

    | (name, Some({outputQuantity}), Some(leftover))
        when leftover >= chemical.quantity =>
      Printf.printf(
        "\nUsing leftovers for %s: %d >= %d, requested %d\n",
        name,
        leftover,
        outputQuantity,
        chemical.quantity,
      );
      leftovers :=
        updateLeftovers(leftovers^, name, _ => leftover - chemical.quantity);
      // we didn't create any ore here
      0;

    | (name, Some({inputs, outputQuantity}), _) =>
      Js.log3("found", inputs, outputQuantity);
      let createdAmount =
        inputs
        |> List.fold_left(
             (totalOre, inputChemical) => {
               Js.log2("IC", inputChemical);
               let createdOre =
                 getOreFor(inputChemical, reactions, ~leftovers);

               Js.log(
                 Printf.sprintf(
                   "  --- Created %d %s, ore %d",
                   inputChemical.quantity,
                   inputChemical.name,
                   createdOre,
                 ),
               );

               totalOre + createdOre;
             },
             0,
           );
      Js.log(
        Printf.sprintf(
          "Requested %d %s, created %d, out %d",
          chemical.quantity,
          name,
          createdAmount,
          outputQuantity,
        ),
      );
      if (outputQuantity >= chemical.quantity) {
        let leftover = outputQuantity - chemical.quantity;
        leftovers := updateLeftovers(leftovers^, name, l => l + leftover);
        createdAmount;
      } else {
        let remaining = chemical.quantity - outputQuantity;
        let chem = {name, quantity: remaining};
        Js.log(
          Printf.sprintf("Not enough %s, need %d more", name, remaining),
        );
        createdAmount + getOreFor(chem, reactions, ~leftovers);
      };
    };
  };
};

let ore = Reaction.getOreFor({name: "FUEL", quantity: 1}, map);

Js.log2("Part 1: ", ore);