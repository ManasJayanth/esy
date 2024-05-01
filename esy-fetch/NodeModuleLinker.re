open EsyPackageConfig;
open RunAsync.Syntax;

/**

   Makes sure we dont link opam packages in node_modules

 */
let getNPMChildren = (~solution, ~fetchDepsSubset, ~node) => {
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

// hardlink the children from local store to node_modules
let rec getPathsToLink' =
        (
          visitedMap,
          ~installation,
          ~projectPath,
          ~solution,
          ~fetchDepsSubset,
          ~rootPackageID,
          ~queue,
        ) => {
  switch (queue |> Queue.take_opt) {
  | Some((pkgID, nodeModulesPath)) =>
    let visited =
      visitedMap
      |> PackageId.Map.find_opt(pkgID)
      |> Stdlib.Option.value(~default=false);
    let* (visitedMap, queue) =
      switch (visited) {
      | false =>
        let visitedMap =
          PackageId.Map.update(pkgID, _ => Some(true), visitedMap);
        let node = Solution.getExn(solution, pkgID);
        let children = getNPMChildren(~solution, ~fetchDepsSubset, ~node);
        let f = childNode => {
          let name = childNode.Package.name;
          let nodeModulesPath = Path.(nodeModulesPath / name / "node_modules");
          let pkgID = childNode.Package.id;
          let visited =
            visitedMap
            |> PackageId.Map.find_opt(pkgID)
            |> Stdlib.Option.value(~default=false);
          if (!visited) {
            Queue.add((pkgID, nodeModulesPath), queue);
          };
        };
        List.iter(~f, children);
        let f = childNode => {
          let* () =
            RunAsync.ofLwt @@
            Esy_logs_lwt.debug(m =>
              m("NodeModuleLinker: processing %a", Package.pp, childNode)
            );

          let pkgID = childNode.Package.id;
          let src = Installation.findExn(pkgID, installation);
          let dst = Path.(nodeModulesPath / childNode.Package.name);
          // figure if a package is JS or esy package
          // Packages from NPM could contain, not just JS, but any natively compiled
          // library
          // Exploring Solver.re to figure this out
          // Cant seem to find an easy way to turn a path to InstallManifest.re
          // childNode.source isn't useful as it only tells if a package is opam or not
          // Let's do it manually
          let* packageJson = NpmPackageJson.ofDir(src);
          switch (packageJson |> Option.bind(~f=NpmPackageJson.esy)) {
          | Some(_) =>
            let* () =
              RunAsync.ofLwt @@
              Esy_logs_lwt.debug(m =>
                m(
                  "NodeModuleLinker: skipping %a because it's package.json contains 'esy' field",
                  Path.pp,
                  src,
                )
              );
            RunAsync.return();
          | None => Fs.hardlinkPath(~src, ~dst)
          };
        };
        let* () = children |> List.map(~f) |> RunAsync.List.waitAll;
        RunAsync.return((visitedMap, queue));
      | true => RunAsync.return((visitedMap, queue))
      };
    getPathsToLink'(
      visitedMap,
      ~installation,
      ~projectPath,
      ~solution,
      ~fetchDepsSubset,
      ~rootPackageID,
      ~queue,
    );
  | None => RunAsync.return()
  };
};

let getPathsToLink = getPathsToLink'(PackageId.Map.empty);

let link = (~installation, ~solution, ~projectPath, ~fetchDepsSubset) => {
  let root = Solution.root(solution);
  let rootPackageID = root.Package.id;
  let nodeModulesPath = Path.(projectPath / "node_modules");
  let queue = Queue.create();
  Queue.add((rootPackageID, nodeModulesPath), queue);
  getPathsToLink(
    ~installation,
    ~projectPath,
    ~solution,
    ~fetchDepsSubset,
    ~rootPackageID,
    ~queue,
  );
};
