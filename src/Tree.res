open Belt
/* \section{Planar Tree Data Type} */

/* A planar tree consists of nodes of tuples of an element of
   type $\alpha$ and a list of sub planar trees.  Leaf nodes
   have the empty list as their list of sub trees. */
type rec t<'a> =
  | Empty
  | Node('a, list<t<'a>>)

/* \subsection{Planar Tree functions} */

/* \subsubsection{Insertion and Deletion} */

/* [create] : create an empty tree */
let empty = () => Empty

let leaf = a => Node(a, list{})

let node = (n, children) => Node(n, children)

/* [fold]: Fold function for trees. */
let rec fold = (f, value) =>
  switch value {
  | Empty => list{list{}}
  | Node(a, lst) => f(a, List.map(lst, fold(f)))
  }

/* [paths] : list of all paths from root node to other nodes in tree */
let paths = (t: t<'a>): list<list<'a>> => fold((x, lst) =>
    switch lst {
    | list{} => list{list{x}}
    | _ =>
      List.reduce(List.map(lst, a => List.map(a, b => list{x, ...b})), list{list{x}}, List.concat)
    }
  , t)
