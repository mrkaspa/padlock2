open Belt

let padLock = [
  [None, Some(2), Some(3)],
  [Some(4), None, Some(6)],
  [Some(7), Some(8), Some(9)],
]

let getVals = ((i, j)): option<int> =>
  padLock[i]->Option.flatMap(arr => arr[j])->Option.flatMap(res => res)

let moves = list{
  (2, 1),
  ((-2), 1),
  (2, (-1)),
  ((-2), (-1)),
  (1, 2),
  (1, (-2)),
  ((-1), 2),
  ((-1), (-2)),
}

module PairComparator =
  Id.MakeComparable({
    type t = (int, int)
    let cmp = ((a0, a1), (b0, b1)) =>
      switch Pervasives.compare(a0, b0) {
      | 0 => Pervasives.compare(a1, b1)
      | c => c
      }
  })

let posSolutions = (~cache=?, (i, j) as idx) => {
  let solve = () =>
    List.reduce(
      moves,
      list{},
      (ls, (im, jm)) => {
        let inw = i + im
        let jnw = j + jm
        if (inw >= 0 && inw < 3 && jnw >= 0 && jnw < 3) {
          switch getVals((inw, jnw)) {
          | Some(_) => list{(inw, jnw), ...ls}
          | None => ls
          }
        } else {
          ls
        }
      },
    )
  switch cache {
  | Some(unwrappedCache) =>
    switch Map.get(unwrappedCache, idx) {
    | Some(sols) => (sols, Some(unwrappedCache))
    | None =>
      let sols = solve()
      (sols, Some(Map.set(unwrappedCache, idx, sols)))
    }
  | None => (solve(), None)
  }
}

let rec findCombinations = (~depth=0, ~cache=?, (i, j) as idx, max_depth) =>
  if (depth > max_depth) {
    switch getVals((i, j)) {
    | Some(_) => Tree.leaf((depth, idx))
    | None => Empty
    }
  } else {
    let (pos, cache) = posSolutions(~cache?, idx)
    let children =
      List.reduce(
        pos,
        list{},
        (nodes, elem) => {
          let child =
            findCombinations(~cache?, ~depth=depth + 1, elem, max_depth)
          switch child {
          | Node(_) as inner => list{inner, ...nodes}
          | _ => nodes
          }
        },
      )

    Tree.node((depth, idx), children)
  }

let map = Map.make(~id=module(PairComparator))
findCombinations(~cache=map, (0, 2), 4)
->Tree.paths
->List.keep(ls => List.length(ls) == 4)
->List.forEach(path => {
    print_string("Path >>\n")
    path
    ->List.map(((_, (i, j))) => getVals((i, j)))
    ->List.forEach(
        (n) => switch n {
        | Some(n) => Js.log(n)
        | None => ()
        }
      )
  })
