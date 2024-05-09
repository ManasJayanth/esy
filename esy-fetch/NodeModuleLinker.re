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

let rec makeHoistedGraph = (~data, ~revLineage) => {
  let parent =
    switch (revLineage) {
    | [] => None
    | [single] =>
      Some(lazy(HoistedGraph.makeNode(~data=single, ~parent=None)))
    | [h, ...rest] =>
      Some(lazy(makeHoistedGraph(~data=h, ~revLineage=rest)))
    };
  HoistedGraph.makeNode(~data, ~parent);
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

let hoistedGraphNodeOfSolutionNode = solutionGraphNode => {
  let SolutionGraph.{parents, data} = solutionGraphNode;
  // TODO rename [parents] to [parentsInReverse]
  // even better: make lineage computation lazy here too
  let revLineageData =
    List.map(parents, ~f=(solutionGraphNode: SolutionGraph.node) =>
      solutionGraphNode.data
    );
  makeHoistedGraph(~data, ~revLineage=revLineageData);
};

let rec iterateSolution = (~traverse, ~hoistedGraph, iterableSolution) => {
  switch (SolutionGraph.take(~traverse, iterableSolution)) {
  | Some((node, nextIterable)) =>
    let nodeModuleEntry = hoistedGraphNodeOfSolutionNode(node);
    let lineage =
      node.parents |> List.map(~f=hoistedGraphNodeOfSolutionNode) |> List.rev;
    let hoistedGraph =
      HoistingAlgorithm.hoistLineage(
        ~lineage,
        ~hoistedGraph,
        nodeModuleEntry,
      );
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
        let nodeModulesPath =
          nodeModulesPathFromParent(
            ~baseNodeModulesPath=Path.(projectPath / "node_modules"),
            hoistedGraphNode,
          );
        print_endline(
          Format.asprintf(
            "Node %a will be installed at %a",
            Package.pp,
            hoistedGraphNode.data,
            Path.pp,
            nodeModulesPath,
          ),
        );
        installPkg(~installation, ~nodeModulesPath, hoistedGraphNode.data);
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
  |> iterateSolution(~traverse, ~hoistedGraph=HoistedGraph.init(~traverse))
  |> HoistedGraph.walk(~f);
};
