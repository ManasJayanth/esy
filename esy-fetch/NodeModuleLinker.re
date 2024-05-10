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

module Data = {
  include Package;
  let sameVersion = (a, b) =>
    EsyPackageConfig.Version.compare(a.version, b.version);
};

module HoistingAlgorithm =
  HoistingAlgorithm.Make(Data, HoistedNodeModulesGraph);

let _debug = (~node) => HoistedNodeModulesGraph.nodePp(node);

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

module SolutionGraphLineage = Lineage.Make(SolutionGraph);
let rec iterateSolution = (~traverse, ~hoistedGraph, iterableSolution) => {
  switch (SolutionGraph.take(~traverse, iterableSolution)) {
  | Some((node, nextIterable)) =>
    let SolutionGraph.{data, parent} = node;
    print_endline(
      Format.asprintf(
        "Solution Node %a being processed",
        SolutionGraph.nodePp,
        node,
      ),
    );
    let nodeModuleEntry =
      HoistedNodeModulesGraph.makeNode(~data, ~parent=None);
    let hoistedGraph =
      switch (parent) {
      | Some(_parent) =>
        let lineage =
          node
          |> SolutionGraphLineage.constructLineage
          |> List.map(~f=solutionGraphNode =>
               solutionGraphNode.SolutionGraph.data
             );
        print_endline(
          Format.asprintf(
            "Here with node %a with lineage length %d",
            SolutionGraph.nodePp,
            node,
            List.length(lineage),
          ),
        );
        HoistingAlgorithm.hoistLineage(
          ~lineage,
          ~hoistedGraph,
          nodeModuleEntry,
        );
      | None =>
        HoistedNodeModulesGraph.addRoot(~node=nodeModuleEntry, hoistedGraph)
      };
    iterateSolution(~traverse, ~hoistedGraph, nextIterable);
  | None => hoistedGraph
  };
};

module NodeModuleLineage = Lineage.Make(HoistedNodeModulesGraph);
let rec nodeModulesPathFromParent = (~baseNodeModulesPath, parent) => {
  switch (HoistedNodeModulesGraph.parent(parent)) {
  | Some(_grandparent) =>
    let lineage = NodeModuleLineage.constructLineage(parent); // This lineage is a list starting from oldest ancestor
    let lineage = List.tl(lineage); // skip root which is just parent id
    let init = baseNodeModulesPath;
    let f = (acc, node) => {
      Path.(
        acc
        / NodeModule.name(node.HoistedNodeModulesGraph.data)
        / "node_modules"
      );
    };
    List.fold_left(lineage, ~f, ~init);
  | None => /* most likely case. ie all pkgs are directly under root  */ baseNodeModulesPath
  };
};

let rec iterateHoistedNodeModulesGraph = (~f, iterableGraph) => {
  switch (HoistedNodeModulesGraph.take(iterableGraph)) {
  | Some((node, nextIterable)) =>
    let* () = f(node);
    iterateHoistedNodeModulesGraph(~f, nextIterable);
  | None => RunAsync.return()
  };
};

let link = (~installation, ~projectPath, ~fetchDepsSubset, ~solution) => {
  let traverse = getNPMChildren(~fetchDepsSubset, ~solution);
  let f = hoistedGraphNode => {
    HoistedNodeModulesGraph.(
      switch (hoistedGraphNode.parent) {
      | Some(_parentHoistedNodeModulesGraphNode) =>
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
  |> iterateSolution(
       ~traverse,
       ~hoistedGraph=HoistedNodeModulesGraph.init(~traverse),
     )
  |> HoistedNodeModulesGraph.iterator
  |> iterateHoistedNodeModulesGraph(~f);
};
