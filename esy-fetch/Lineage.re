module type M = {
  type node;
  let parent: node => option(Lazy.t(node));
};

module type S = {
  type t;
  let constructLineage: t => list(t);
};

module Make = (M: M) : (S with type t := M.node) => {
  let constructLineage' = (acc, parent) => {
    switch (M.parent(parent)) {
    | Some(grandparent) => [Lazy.force(grandparent), ...acc]
    | None => acc
    };
  };

  let constructLineage = constructLineage'([]);
};
