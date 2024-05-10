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
       (
         K: Map.OrderedType,
         HoistedNodeModulesGraph:
           HoistedNodeModulesGraph.S with type data = K.t,
       )

         : (
           S with
             type hoistedGraph = HoistedNodeModulesGraph.t and
             type hoistedGraphNode = HoistedNodeModulesGraph.node and
             type data = K.t
       ) => {
  type data = K.t;
  type hoistedGraph = HoistedNodeModulesGraph.t;
  type hoistedGraphNode = HoistedNodeModulesGraph.node;

  let rec proceedMatching = (~root: hoistedGraphNode, ~lineage, ~targetNode) => {
    let children = Lazy.force(root.children);
    switch (lineage) {
    | [] =>
      let dataField = HoistedNodeModulesGraph.nodeData(targetNode);
      switch (HoistedNodeModulesGraph.Map.find_opt(dataField, children)) {
      | Some(_) =>
        // TODO review
        Error("Package with same name exists")
      | None =>
        Ok(
          HoistedNodeModulesGraph.nodeUpdateChildren(
            dataField,
            targetNode,
            root,
          ),
        )
      };
    | [h, ...r] =>
      switch (HoistedNodeModulesGraph.Map.find_opt(h, children)) {
      | Some(child) =>
        switch (proceedMatching(~root=child, ~lineage=r, ~targetNode)) {
        | Ok(newChild) =>
          Ok(HoistedNodeModulesGraph.nodeUpdateChildren(h, newChild, root))
        | Error(e) => Error(e)
        }
      | None =>
        let child =
          HoistedNodeModulesGraph.makeNode(
            ~parent=Some(lazy(root)),
            ~data=h,
          );
        let updatedRoot =
          HoistedNodeModulesGraph.nodeUpdateChildren(h, child, root);
        proceedMatching(
          ~root=updatedRoot /* hack: we're calling this fn again */,
          ~lineage=[h, ...r],
          ~targetNode,
        );
      }
    };
  };

  let hoist = (~hypotheticalLineage, ~hoistedGraph, node) => {
    let roots = HoistedNodeModulesGraph.roots(hoistedGraph);
    switch (hypotheticalLineage) {
    | [h, ...rest] =>
      switch (HoistedNodeModulesGraph.Map.find_opt(h, roots)) {
      | Some(hoistedGraphRoot) =>
        switch (
          proceedMatching(
            ~root=hoistedGraphRoot,
            ~lineage=rest,
            ~targetNode=node,
          )
        ) {
        | Ok(newRoot) =>
          HoistedNodeModulesGraph.Map.update(h, _ => Some(newRoot), roots)
          |> HoistedNodeModulesGraph.ofRoots
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
     We got rid of Queue.t since we endup traversing it immmediately. It was stateful and
     tricky to reason about.
       [hypotheticalLineage] can initially be empty, but should never return empty


     TODO: lineage == [] could mean,
     1. we finished processing lineage
     2. node has no parents

     This is unfortunate. And needs a hack to work around. Lazy lineage computation would solve this.
 */
  let rec hoistLineage' =
          (~hypotheticalLineage, ~lineage, ~hoistedGraph, node) => {
    switch (lineage) {
    | [head, ...rest] =>
      let hypotheticalLineage = hypotheticalLineage @ [head];
      switch (hoist(~hypotheticalLineage, ~hoistedGraph, node)) {
      | Ok(hoistedGraph) => hoistedGraph
      | Error(msg) =>
        print_endline(
          Format.asprintf(
            "Couldn't hoist %a \nBecause: %s\nLineage: %a",
            HoistedNodeModulesGraph.nodePp,
            node,
            msg,
            HoistedNodeModulesGraph.dataListPp,
            hypotheticalLineage,
          ),
        );
        // Workaround to make sure we dont recurse as we have finished through
        // the lineage. See notes in docstring
        if (List.length(rest) > 0) {
          hoistLineage'(
            ~hypotheticalLineage,
            ~hoistedGraph,
            ~lineage=rest,
            node,
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
      HoistedNodeModulesGraph.addRoot(~node, hoistedGraph)
    };
  };
  let hoistLineage = hoistLineage'(~hypotheticalLineage=[]);
};
