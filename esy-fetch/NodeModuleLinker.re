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

let rec iterateSolution = (~traverse, ~hoistedGraph, iterableSolution) => {
  switch (SolutionGraph.take(~traverse, iterableSolution)) {
  | Some((node, nextIterable)) =>
    let SolutionGraph.{data, parents /* actually parents in reverse */} = node;
    let nodeModuleEntry = HoistedGraph.makeNode(~data, ~parent=None);
    let lineage =
      parents
      |> List.map(~f=solutionNode => solutionNode.SolutionGraph.data)
      |> List.rev;
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
