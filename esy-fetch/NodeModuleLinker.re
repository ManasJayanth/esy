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

let installPkg = (~installation, ~nodeModulesPath, childNode) => {
  let* () =
    RunAsync.ofLwt @@
    Esy_logs_lwt.debug(m =>
      m("NodeModuleLinker: installing %a", Package.pp, childNode)
    );
  let pkgID = childNode.Package.id;
  let src = Installation.findExn(pkgID, installation);
  let dst = Path.(nodeModulesPath / childNode.Package.name);
  Fs.hardlinkPath(~src, ~dst);
};

/**

   Install the children from esy store to node_modules

   Notes:
   Doesn't install the current node itself. Just the children.

 */
let rec linkDependencies =
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
        let node = Solution.getExn(solution, pkgID);
        let children = getNPMChildren(~solution, ~fetchDepsSubset, ~node);
        let f = (visitedMap, childNode) => {
          let name = childNode.Package.name;
          let nodeModulesPath = Path.(nodeModulesPath / name / "node_modules");
          let pkgID = childNode.Package.id;
          let visited =
            visitedMap
            |> PackageId.Map.find_opt(pkgID)
            |> Stdlib.Option.value(~default=false);
          if (!visited) {
            Queue.add((pkgID, nodeModulesPath), queue);
            PackageId.Map.update(pkgID, _ => Some(true), visitedMap);
          } else {
            visitedMap;
          };
        };
        let visitedMap = List.fold_left(~f, ~init=visitedMap, children);
        let* () =
          children
          |> List.map(~f=node => {
               let visited =
                 visitedMap
                 |> PackageId.Map.find_opt(pkgID)
                 |> Stdlib.Option.value(~default=false);
               if (!visited) {
                 installPkg(~installation, ~nodeModulesPath, node);
               } else {
                 RunAsync.return();
               };
             })
          |> RunAsync.List.waitAll;
        RunAsync.return((visitedMap, queue));
      | true => RunAsync.return((visitedMap, queue))
      };
    linkDependencies(
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

let rec findDuplicatedDeps =
        (~fetchDepsSubset, ~solution, (seenIDs, duplicates), queue) => {
  switch (Queue.take_opt(queue)) {
  | Some(node) =>
    print_endline(
      Format.asprintf("findDuplicatedDeps: %a", Package.pp, node),
    );
    if (PackageId.Set.mem(node.Package.id, seenIDs)) {
      findDuplicatedDeps(
        ~fetchDepsSubset,
        ~solution,
        (seenIDs, Package.Set.add(node, duplicates)),
        queue,
      );
    } else {
      let seenIDs = PackageId.Set.add(node.Package.id, seenIDs);
      getNPMChildren(~fetchDepsSubset, ~solution, ~node)
      |> List.filter(~f=node => !PackageId.Set.mem(node.Package.id, seenIDs))
      |> List.iter(~f=node => Queue.add(node, queue));
      findDuplicatedDeps(
        ~fetchDepsSubset,
        ~solution,
        (seenIDs, duplicates),
        queue,
      );
    };
  | None => (seenIDs, duplicates)
  };
};

let link = (~installation, ~solution, ~projectPath, ~fetchDepsSubset) => {
  let root = Solution.root(solution);
  let rootPackageID = root.Package.id;
  let nodeModulesPath = Path.(projectPath / "node_modules");
  let duplicatesQueue = Queue.create();
  Queue.add(Solution.root(solution), duplicatesQueue);
  let (_seen, duplicateDepsSet) =
    findDuplicatedDeps(
      ~fetchDepsSubset,
      ~solution,
      (PackageId.Set.empty /* seen */, Package.Set.empty /* duplicated */),
      duplicatesQueue,
    );
  let duplicateDeps = Package.Set.elements(duplicateDepsSet);
  let queue = Queue.create();
  let* () =
    duplicateDeps
    |> List.map(~f=pkg => {
         let* () =
           RunAsync.ofLwt @@
           Esy_logs_lwt.debug(m =>
             m("NodeModuleLinker: hoisting %a", Package.pp, pkg)
           );
         Queue.add((pkg.Package.id, nodeModulesPath), queue);
         installPkg(~installation, ~nodeModulesPath, pkg);
       })
    |> RunAsync.List.waitAll;
  Queue.add((rootPackageID, nodeModulesPath), queue);
  /* let visitedMap = */
  /*   duplicateDeps */
  /*   |> List.fold_left( */
  /*        ~init=PackageId.Map.empty, ~f=(visitedMap, duplicatedPkg) => */
  /*        PackageId.Map.add(duplicatedPkg.Package.id, true, visitedMap) */
  /*      ); */
  linkDependencies(
    PackageId.Map.empty,
    ~installation,
    ~projectPath,
    ~solution,
    ~fetchDepsSubset,
    ~rootPackageID,
    ~queue,
  );
};
