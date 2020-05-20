open Belt;

let padLock = [|
  [|None, Some(2), Some(3)|],
  [|Some(4), None, Some(6)|],
  [|Some(7), Some(8), Some(9)|],
|];

let getVals = ((i, j)): option(int) =>
  padLock[i]->Option.flatMap(arr => arr[j])->Option.flatMap(res => res);

let moves = [
  (2, 1),
  ((-2), 1),
  (2, (-1)),
  ((-2), (-1)),
  (1, 2),
  (1, (-2)),
  ((-1), 2),
  ((-1), (-2)),
];

module PairComparator =
  Id.MakeComparable({
    type t = (int, int);
    let cmp = ((a0, a1), (b0, b1)) =>
      switch (Pervasives.compare(a0, b0)) {
      | 0 => Pervasives.compare(a1, b1)
      | c => c
      };
  });

let posSolutions = ((i, j) as idx, cache) =>
  switch (Map.get(cache, idx)) {
  | Some(sols) => (sols, cache)
  | None =>
    let sols =
      List.reduce(
        moves,
        [],
        (ls, (im, jm)) => {
          let inw = i + im;
          let jnw = j + jm;
          if (inw >= 0 && inw < 3 && jnw >= 0 && jnw < 3) {
            switch (getVals((inw, jnw))) {
            | Some(_) => [(inw, jnw), ...ls]
            | None => ls
            };
          } else {
            ls;
          };
        },
      );

    (sols, Map.set(cache, idx, sols));
  };

let rec findCombinations = (~depth=0, ~cache, (i, j) as idx, max_depth) =>
  if (depth > max_depth) {
    switch (getVals((i, j))) {
    | Some(_) => Tree.leaf((depth, idx))
    | None => Empty
    };
  } else {
    let (pos, cache) = posSolutions(idx, cache);
    let children =
      List.reduce(
        pos,
        [],
        (nodes, elem) => {
          let child =
            findCombinations(~cache, ~depth=depth + 1, elem, max_depth);
          switch (child) {
          | Node(_) as inner => [inner, ...nodes]
          | _ => nodes
          };
        },
      );

    Tree.node((depth, idx), children);
  };

let map = Map.make(~id=(module PairComparator));
findCombinations(~cache=map, (0, 2), 4)
->Tree.paths
->List.keep(ls => List.length(ls) == 4)
->List.forEach(path => {
    print_string("Path >>\n");
    path
    ->List.map(((_, (i, j))) => getVals((i, j)))
    ->List.forEach(
        fun
        | Some(n) => Js.log(n)
        | None => (),
      );
  });
