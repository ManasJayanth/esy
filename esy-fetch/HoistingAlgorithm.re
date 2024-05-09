module type S = {
  type data;
  /** Represents the hoisted node_module graph. Each node's id is the package name (not version) */
  type hoistedGraph;
  /** Represents the entry/node in hoisted node_module. See [hoistedGraph('a)] for more details */
  type hoistedGraphNode;

  /** Takes a named list [hypotheticalLineage] of 'a, a [hoistedGraph('a)] named [hoistedGraph], and a node which has a type which can be [hoistedGraph]'s node (but not necessarily so), and returns if it can become a node indeed */
  let hoist:
    (
      ~hypotheticalLineage: list(data),
      ~hoistedGraph: hoistedGraph,
      hoistedGraphNode
    ) =>
    result(hoistedGraph, string);

  let hoistLineage:
    (~lineage: list(data), ~hoistedGraph: hoistedGraph, hoistedGraphNode) =>
    hoistedGraph;
};

module Make =
       (K: Map.OrderedType, HoistedGraph: HoistedGraph.S with type data = K.t)

         : (
           S with
             type hoistedGraph = HoistedGraph.t and
             type hoistedGraphNode = HoistedGraph.node and
             type data = K.t
       ) => {
  type data = K.t;
  type hoistedGraph = HoistedGraph.t;
  type hoistedGraphNode = HoistedGraph.node;

  let rec proceedMatching = (~root: hoistedGraphNode, ~lineage, ~targetNode) => {
    let children = Lazy.force(root.children);
    switch (lineage) {
    | [] =>
      let dataField = HoistedGraph.nodeData(targetNode);
      switch (HoistedGraph.Map.find_opt(dataField, children)) {
      | Some(_) =>
        // TODO review
        Error("Cant hoist: package with same name exists")
      | None =>
        Ok(HoistedGraph.nodeUpdateChildren(dataField, targetNode, root))
      };
    | [h, ...r] =>
      switch (HoistedGraph.Map.find_opt(h, children)) {
      | Some(child) =>
        switch (proceedMatching(~root=child, ~lineage=r, ~targetNode)) {
        | Ok(newChild) =>
          Ok(HoistedGraph.nodeUpdateChildren(h, newChild, root))
        | Error(e) => Error(e)
        }
      | None =>
        let child = HoistedGraph.makeNode(~parent=Some(lazy(root)), ~data=h);
        let updatedRoot = HoistedGraph.nodeUpdateChildren(h, child, root);
        proceedMatching(
          ~root=updatedRoot /* hack: we're calling this fn again */,
          ~lineage=[h, ...r],
          ~targetNode,
        );
      }
    };
  };

  let hoist = (~hypotheticalLineage, ~hoistedGraph, node) => {
    let roots = HoistedGraph.roots(hoistedGraph);
    switch (hypotheticalLineage) {
    | [h, ...rest] =>
      switch (HoistedGraph.Map.find_opt(h, roots)) {
      | Some(hoistedGraphRoot) =>
        switch (
          proceedMatching(
            ~root=hoistedGraphRoot,
            ~lineage=rest,
            ~targetNode=node,
          )
        ) {
        | Ok(newRoot) =>
          HoistedGraph.Map.update(h, _ => Some(newRoot), roots)
          |> HoistedGraph.ofRoots
          |> Result.return
        | Error(e) => Error(e)
        }
      | None => Error("Lineage doesn't start with known roots")
      }
    | [] => Error("hypotheticalLineage should not be empty")
    };
  };

  /**
       Notes:
       [parentsSoFar] is a queue because we need fast appends as well has forward traversal
       [hypotheticalLineage] can initially be empty, but should never return empty


     TODO: lineage == [] could mean,
     1. we finished processing lineage
     2. node has no parents

     This is unfortunate. And needs a hack to work around. Lazy lineage computation would solve this.
 */
  let rec hoistLineage' = (~hypotheticalLineage, ~hoistedGraph, ~lineage, pkg) => {
    switch (lineage) {
    | [head, ...rest] =>
      Queue.push(head, hypotheticalLineage);
      // TODO: get rid of queue. We end up traversing right after
      // creating it.
      let hypotheticalLineageList =
        hypotheticalLineage |> Queue.to_seq |> List.of_seq;
      switch (
        hoist(
          ~hypotheticalLineage=hypotheticalLineageList,
          ~hoistedGraph,
          pkg,
        )
      ) {
      | Ok(hoistedGraph) => hoistedGraph
      | Error(msg) =>
        print_endline(
          Format.asprintf(
            "Couldn't hoist %a: because: %s",
            HoistedGraph.nodePp,
            pkg,
            msg,
          ),
        );
        // Workaround to make sure we dont recurse as we have finished through
        // the lineage. See notes in docstring
        if (List.length(rest) > 0) {
          hoistLineage'(
            ~hypotheticalLineage,
            ~hoistedGraph,
            ~lineage=rest,
            pkg,
          );
        } else {
          hoistedGraph;
        };
      };
    | [] =>
      // HACK! TODO remove this
      // This should only be done when a node has empty lineage.
      // not because we kept recursing and ran out of lineage.
      // See notes in the docstring
      HoistedGraph.addRoot(~node=pkg, hoistedGraph)
    };
  };
  let hoistLineage = (~lineage, ~hoistedGraph, pkg) => {
    hoistLineage'(
      ~hypotheticalLineage=Queue.create(),
      ~lineage,
      ~hoistedGraph,
      pkg,
    );
  };
};
