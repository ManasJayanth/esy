/**
   Makes sure we dont link opam packages in node_modules
 */
let getNPMChildren = (~solution, ~fetchDepsSubset, node) => {
  let f = (pkg: Solution.pkg) => {
    switch (pkg.Package.version) {
    | Opam(_) => false
    | Npm(_)
    | Source(_) => true
    /*
        Allowing sources here would let us resolve to github urls for
        npm dependencies. Atleast in theory. TODO: test this
     */
    };
  };
  Solution.dependenciesBySpec(solution, fetchDepsSubset, node)
  |> List.filter(~f);
};
/* let installPkg = (~installation, ~nodeModulesPath, childNode) => { */
/*   print_endline( */
/*     Format.asprintf("Linking %a \n---- ", Package.pp, childNode), */
/*   ); */
/*   let* () = */
/*     RunAsync.ofLwt @@ */
/*     Esy_logs_lwt.debug(m => */
/*       m("NodeModuleLinker: installing %a", Package.pp, childNode) */
/*     ); */
/*   let pkgID = childNode.Package.id; */
/*   let src = Installation.findExn(pkgID, installation); */
/*   let dst = Path.(nodeModulesPath / childNode.Package.name); */
/*   Fs.hardlinkPath(~src, ~dst); */
/* }; */
module HoistedGraph = {
  type data = Package.t;
  module Map = Map.Make(Package);
  type t = Map.t(node)
  and node = {
    parent: option(Lazy.t(node)),
    data: Package.t,
    children: Lazy.t(Map.t(node)),
  };
  let roots = roots => roots;
  let empty = Map.empty;
  let ofRoots = roots => roots;
  let nodeUpdateChildren = (dataField, newNode, parent) => {
    {
      ...parent,
      children:
        lazy(Map.add(dataField, newNode, Lazy.force(parent.children))),
    };
  };
  let nodeData = node => node.data;

  let makeCache: Hashtbl.t(data, node) = Hashtbl.create(100);
  let rec makeNode' = (~traverse, ~parent, ~data) => {
    let init = Map.empty;
    let f = (acc, child) => {
      Map.add(
        child,
        makeNode(
          ~traverse,
          ~parent=Some(lazy(makeNode(~traverse, ~parent, ~data))),
          ~data=child,
        ),
        acc,
      );
    };
    {
      parent,
      data,
      children: lazy(List.fold_left(~f, ~init, traverse(data))),
    };
  }
  and makeNode:
    (
      ~traverse: Solution.pkg => list(Solution.pkg),
      ~parent: option(Lazy.t(node)),
      ~data: data
    ) =>
    node =
    (~traverse, ~parent, ~data) => {
      switch (Hashtbl.find_opt(makeCache, data)) {
      | Some(node) => node
      | None => makeNode'(~traverse, ~parent, ~data)
      };
    };
};

let rec makeHoistedGraph = (~traverse, ~data, ~revLineage) => {
  let parent =
    switch (revLineage) {
    | [] => None
    | [single] =>
      Some(
        lazy(HoistedGraph.makeNode(~traverse, ~data=single, ~parent=None)),
      )
    | [h, ...rest] =>
      Some(lazy(makeHoistedGraph(~traverse, ~data=h, ~revLineage=rest)))
    };
  HoistedGraph.makeNode(~traverse, ~data, ~parent);
};

module HoistingAlgorithm = HoistingAlgorithm.Make(Package, HoistedGraph);

let _debugHoist = (~node, ~lineage) => {
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

let hoistedGraphNodeOfSolutionNode = (~traverse, solutionGraphNode) => {
  open NodeModule;
  let SolutionGraph.{parents, data} = solutionGraphNode;
  // TODO rename [parents] to [parentsInReverse]
  let revLineageData =
    List.map(parents, ~f=(solutionGraphNode: SolutionGraph.node) =>
      solutionGraphNode.data
    );
  makeHoistedGraph(~traverse, ~data, ~revLineage=revLineageData);
};

let rec iterateSolution = (~traverse, ~hoistedGraph, iterableSolution) => {
  NodeModule.(
    switch (SolutionGraph.take(~traverse, iterableSolution)) {
    | Some((node, nextIterable)) =>
      let nodeModuleEntry = hoistedGraphNodeOfSolutionNode(~traverse, node);
      let lineage =
        node.parents
        |> List.map(~f=hoistedGraphNodeOfSolutionNode(~traverse))
        |> List.rev;
      let hoistedGraph =
        hoistLineage(~lineage, ~hoistedGraph, nodeModuleEntry);
      /* debugHoist(~node=nodeModuleEntry, ~lineage=hoistedLineage); */
      iterateSolution(~traverse, ~hoistedGraph, nextIterable);
    | None => RunAsync.return() /* TODO should return hoistedGraph */
    }
  );
};

let link = (~fetchDepsSubset, ~solution) => {
  open NodeModule;
  let traverse = getNPMChildren(~fetchDepsSubset, ~solution);
  let hoistedGraph = HoistedGraph.empty;
  solution
  |> SolutionGraph.iterator
  |> iterateSolution(~traverse, ~hoistedGraph);
};
