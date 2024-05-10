module type M = {
  type node;
  let parent: node => option(Lazy.t(node));
};

module type S = {
  type t;
  let constructLineage: t => list(t);
};

module Make = (M: M) : (S with type t := M.node) => {
  let rec constructLineage' = (acc, node) => {
    switch (M.parent(node)) {
    | Some(parent) =>
      let parent = Lazy.force(parent);
      constructLineage'([parent, ...acc], parent);
    | None => List.rev(acc)
    };
  };

  /** Returns a list of paents (lineage) starting from oldest ancestor first */
  let constructLineage = constructLineage'([]);
};
