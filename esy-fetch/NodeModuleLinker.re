module HoistedGraph = {
  type data = Package.t;
  module Map = Map.Make(Package);
  type t = Map.t(node)
  and node = {
    parent: node,
    data: Package.t,
    children: Map.t(node),
  };
  let roots = roots => roots;
  let ofRoots = roots => roots;
  let nodeUpdateChildren = (dataField, newNode, parent) => {
    {...parent, children: Map.add(dataField, newNode, parent.children)};
  };
  let nodeData = node => node.data;
  let children = node => node.children;
};

module HoistingAlgorithm = HoistingAlgorithm.Make(Package, HoistedGraph);

let debugHoist = (~node, ~lineage) => {
  NodeModule.(
    if (List.length(lineage) > 0) {
      print_endline(
        Format.asprintf(
          "Node %a will be hoisted to %a",
          Package.pp,
          node.SolutionGraph.data,
          SolutionGraph.parentsPp,
          lineage,
        ),
      );
    } else {
      print_endline(
        Format.asprintf(
          "Node %a will be hoisted to <root>",
          Package.pp,
          node.NodeModule.SolutionGraph.data,
        ),
      );
    }
  );
};

/**
       Notes:
       [parentsSoFar] is a queue because we need fast appends as well has forward traversal
       [hypotheticalLineage] can initially be empty, but should never return empty
 */
let hoistLineage = (~lineage, ~hoistedGraph, pkg) => {
  let rec aux = (~hypotheticalLineage, ~hoistedGraph, ~lineage, pkg) => {
    switch (lineage) {
    | [head, ...rest] =>
      Queue.push(head, hypotheticalLineage);
      // TODO: get rid of queue. We end up traversing right after
      // creating it.
      let hypotheticalLineageList =
        hypotheticalLineage
        |> Queue.to_seq
        |> List.of_seq
        |> List.map(~f=node => HoistedGraph.nodeData(node));
      switch (
        HoistingAlgorithm.hoist(
          ~hypotheticalLineage=hypotheticalLineageList,
          ~hoistedGraph,
          pkg,
        )
      ) {
      | Ok(hoistedGraph) => hoistedGraph
      | Error(_) =>
        aux(~hypotheticalLineage, ~hoistedGraph, ~lineage=rest, pkg)
      };
    | [] => failwith("Cannot hoist")
    };
  };
  aux(~hypotheticalLineage=Queue.create(), ~lineage, ~hoistedGraph, pkg);
};

let rec iterateSolution = (~traverse, ~hoistedGraph, iterableSolution) => {
  NodeModule.(
    switch (SolutionGraph.take(~traverse, iterableSolution)) {
    | Some((node, nextIterable)) =>
      let SolutionGraph.{parents, _} = node;
      // TODO rename [parents] to [parentsInReverse]
      let nodeModuleEntry = HoistedGraph.nodeOfSolutionGraphNode(node);
      let lineage = List.rev(parents);
      let hoistedLineage =
        hoistLineage(~lineage, ~hoistedGraph, nodeModuleEntry);
      let hoistedGraph =
        debugHoist(
          ~node=nodeModuleEntry,
          ~lineage=hoistedLineage,
          ~graph=hoistedGraph,
        );
      iterateSolution(~traverse, ~hoistedGraph, nextIterable);
    | None => RunAsync.return()
    }
  );
};

let link = (~fetchDepsSubset, ~solution) => {
  open NodeModule;
  open HoistingAlgorithm;
  let traverse = getNPMChildren(~fetchDepsSubset, ~solution);
  let hoistedGraph = HoistedGraph.empty;
  solution
  |> SolutionGraph.iterator
  |> iterateSolution(~traverse, ~hoistedGraph);
};
