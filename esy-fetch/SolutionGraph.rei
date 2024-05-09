/** Abstract value to help provide a iterator API into solution graph */
type state;

type parents = list(node)
// TODO rename [parents] to [parentsInReverse]
// even better: make lineage computation lazy here too
and node = {
  parents,
  data: Solution.pkg,
};

let nodePp: Fmt.t(node);
let parentsPp: Fmt.t(parents);

type traversalFn = Solution.pkg => list(Solution.pkg);

/** Setup a stateful interator */
let iterator: Solution.t => state;

/** Pop from the current item being traversed */
let take: (~traverse: traversalFn, state) => option((node, state));

let debug: (~traverse: traversalFn, Solution.t) => unit;

/* TODO [iterator] doesn't need Solution.t. It just needs a root (which is Package.t). Everything else is lazy (on-demand) */
