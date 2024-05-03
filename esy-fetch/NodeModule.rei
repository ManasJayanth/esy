/**
     These modules represent an entry (a package) in node_modules folder.
     packages in the node_modules folder have be unique by name. JS
     packages in the graph could have more one versions present - we
     have to carefully avoid conflicts and save disk space.


   Usage:


   let isHoistableTo : (parentsLineage) => bool
   */
type t;
let compare: (t, t) => int;
module Map: Map.S with type key := t;
module Set: Set.S with type elt := t;

/* TODO [iterator] doesn't need Solution.t. It just needs a root (which is Package.t). Everything else is lazy (on-demand) */
module SolutionGraph: {
  /** Abstract value to help provide a iterator API into solution graph */
  type state;

  type parents = list(node)
  and node = {
    parents,
    data: Solution.pkg,
    children: Package.Map.t(bool),
  };

  let nodePp: Fmt.t(node);
  let parentsPp: Fmt.t(parents);

  type traversalFn = Solution.pkg => list(Solution.pkg);

  /** Setup a stateful interator */
  let iterator: (~traverse: traversalFn, Solution.t) => state;

  /** Pop from the current item being traversed */
  let take: (~traverse: traversalFn, state) => option((node, state));

  let debug: (~traverse: traversalFn, Solution.t) => unit;
};
