open RunAsync.Syntax;

/**
   Makes sure we dont link opam packages in node_modules
 */
let getNPMChildren = (~solution, ~fetchDepsSubset, node) => {
  let f = (pkg: NodeModule.t) => {
    switch (NodeModule.version(pkg)) {
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

let installPkg = (~installation, ~nodeModulesPath, pkg) => {
  let* () =
    RunAsync.ofLwt @@
    Esy_logs_lwt.debug(m =>
      m("NodeModuleLinker: installing %a", Package.pp, pkg)
    );
  let pkgID = pkg.Package.id;
  let src = Installation.findExn(pkgID, installation);
  let dst = Path.(nodeModulesPath / pkg.Package.name);
  Fs.hardlinkPath(~src, ~dst);
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

let _debug = (~node) => HoistedGraph.nodePp(node);

let _debugHoist = (~node, ~lineage) =>
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
        node.SolutionGraph.data,
      ),
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
    | [] => HoistedGraph.addRoot(~node=pkg, hoistedGraph)
    };
  };
  aux(~hypotheticalLineage=Queue.create(), ~lineage, ~hoistedGraph, pkg);
};

let hoistedGraphNodeOfSolutionNode = (~traverse, solutionGraphNode) => {
  let SolutionGraph.{parents, data} = solutionGraphNode;
  // TODO rename [parents] to [parentsInReverse]
  let revLineageData =
    List.map(parents, ~f=(solutionGraphNode: SolutionGraph.node) =>
      solutionGraphNode.data
    );
  makeHoistedGraph(~traverse, ~data, ~revLineage=revLineageData);
};

let rec iterateSolution = (~traverse, ~hoistedGraph, iterableSolution) => {
  switch (SolutionGraph.take(~traverse, iterableSolution)) {
  | Some((node, nextIterable)) =>
    let nodeModuleEntry = hoistedGraphNodeOfSolutionNode(~traverse, node);
    let lineage =
      node.parents
      |> List.map(~f=hoistedGraphNodeOfSolutionNode(~traverse))
      |> List.rev;
    let hoistedGraph = hoistLineage(~lineage, ~hoistedGraph, nodeModuleEntry);
    iterateSolution(~traverse, ~hoistedGraph, nextIterable);
  | None => hoistedGraph
  };
};

let link = (~installation, ~projectPath, ~fetchDepsSubset, ~solution) => {
  let traverse = getNPMChildren(~fetchDepsSubset, ~solution);
  let f = hoistedGraphNode => {
    HoistedGraph.(
      switch (hoistedGraphNode.parent) {
      | Some(_parentHoistedGraphNode) =>
        print_endline(
          Format.asprintf(
            "Node %a will be hoisted",
            Package.pp,
            hoistedGraphNode.data,
          ),
        );
        installPkg(
          ~installation,
          ~nodeModulesPath=
            nodeModulesPathFromParent(
              ~baseNodeModulesPath=Path.(projectPath / "node_modules"),
              hoistedGraphNode,
            ),
          hoistedGraphNode.data,
        );
      | None =>
        print_endline(
          Format.asprintf(
            "Node %a will be skipped as it is a parent node",
            Package.pp,
            hoistedGraphNode.data,
          ),
        );
        RunAsync.return();
      }
    );
  };
  solution
  |> SolutionGraph.iterator
  |> iterateSolution(~traverse, ~hoistedGraph=HoistedGraph.empty)
  |> HoistedGraph.walk(~f);
};
