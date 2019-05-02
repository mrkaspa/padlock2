open Belt;

let pad_lock = [|
  [|None, Some(2), Some(3)|],
  [|Some(4), None, Some(6)|],
  [|Some(7), Some(8), Some(9)|],
|];

let getVals = ((i, j)) =>
  switch (pad_lock[i]) {
  | Some(new_pad) =>
    switch (new_pad[j]) {
    | None => None
    | res => res
    }
  | None => None
  };

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

let pos_solutions = ((i, j) as idx, cache) =>
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

let rec find_n_combinations = ((i, j) as idx, depth, max_depth, ~cache) =>
  if (depth > max_depth) {
    switch (getVals((i, j))) {
    | Some(_) => Tree.leaf((depth, idx))
    | None => Empty
    };
  } else {
    let (pos, cache) = pos_solutions(idx, cache);
    let children =
      List.reduce(
        pos,
        [],
        (nodes, elem) => {
          let child = find_n_combinations(elem, depth + 1, max_depth, ~cache);
          switch (child) {
          | Node(_) as inner => [inner, ...nodes]
          | _ => nodes
          };
        },
      );

    Tree.node((depth, idx), children);
  };

let map = Map.make(~id=(module PairComparator));
find_n_combinations((0, 2), 0, 4, ~cache=map)
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
