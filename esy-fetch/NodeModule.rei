/**
     These modules represent an entry (a package) in node_modules folder.
     packages in the node_modules folder have be unique by name. JS
     packages in the graph could have more one versions present - we
     have to carefully avoid conflicts and save disk space.


   Usage:

   let linkSolution: (Solution.t, results) => RunAsync.t(unit)
   let linkSolution = (solution, acc) => {
   let iterableSolution = NodeModule.SolutionGraph.iterator(solution);
   loop(iterableSolution, [])



   let rec loop = (iterableSolution, results) => {
   switch (SolutionGraph.take(iterableSolution) {
   | Some((node, nextIterSolution)) => {
   let { parents, children, data } = node;
   List.fold_left(~f=(parentsSeenSoFar, parent) => {
   if (isHoistableTo(parentsSeenSoFar)) {
    parentsSeenSoFar
   } else {
   parentSeenSoFar @ [parent]
   }
   }, [])
   | None => results
   }


   let isHoistableTo : (parentsLineage) => bool
   */
type t;
let compare: (t, t) => int;
module Map: Map.S with type key := t;
module Set: Set.S with type elt := t;

module SolutionGraph: {
  /** Abstract value to help provide a iterator API into solution graph */
  type state;

  type node = {
    parents: list(Solution.pkg),
    data: Solution.pkg,
    children: Map.t(bool),
  };

  type traversalFn = Solution.pkg => list(Solution.pkg);

  /** Setup a stateful interator */
  let iterator: (~traverse: traversalFn, Solution.t) => state;

  /** Pop from the current item being traversed */
  let take: (~traverse: traversalFn, state) => option(node);
};
