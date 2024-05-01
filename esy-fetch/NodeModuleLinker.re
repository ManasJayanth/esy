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
  print_endline(
    Format.asprintf("Linking %a \n---- ", Package.pp, childNode),
  );
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
  | Some((node, nodeModulesPath)) =>
    print_endline(Format.asprintf("Dequeue %a \n---- ", Package.pp, node));
    let* (visitedMap, queue) = {
      let children = getNPMChildren(~solution, ~fetchDepsSubset, ~node);
      let* () =
        children
        |> List.map(~f=node => {
             let visited =
               visitedMap
               |> PackageId.Map.find_opt(node.Package.id)
               |> Stdlib.Option.value(~default=false);
             if (!visited) {
               installPkg(~installation, ~nodeModulesPath, node);
             } else {
               RunAsync.return();
             };
           })
        |> RunAsync.List.waitAll;
      let f = (visitedMap, childNode) => {
        let name = childNode.Package.name;
        let nodeModulesPath = Path.(nodeModulesPath / name / "node_modules");
        let pkgID = childNode.Package.id;
        let visited =
          visitedMap
          |> PackageId.Map.find_opt(pkgID)
          |> Stdlib.Option.value(~default=false);
        if (!visited) {
          Queue.add((childNode, nodeModulesPath), queue);
          print_endline(
            Format.asprintf("Enqueue %a \n---- ", Package.pp, childNode),
          );
          PackageId.Map.update(pkgID, _ => Some(true), visitedMap);
        } else {
          visitedMap;
        };
      };
      let visitedMap = List.fold_left(~f, ~init=visitedMap, children);
      RunAsync.return((visitedMap, queue));
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
      Format.asprintf("Dequeue(findDuplicatedDeps): %a", Package.pp, node),
    );
    if (PackageId.Set.mem(node.Package.id, seenIDs)) {
      print_endline(Format.asprintf("found: %a", Package.pp, node));
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
      |> List.iter(~f=node => {
           print_endline(
             Format.asprintf(
               "Enqueue(findDuplicatedDeps) %a \n---- ",
               Package.pp,
               node,
             ),
           );
           Queue.add(node, queue);
         });
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
  Queue.add(root, duplicatesQueue);
  print_endline(
    Format.asprintf(
      "Enqueue(findDuplicatedDeps) %a \n---- ",
      Package.pp,
      root,
    ),
  );
  let (_seen, duplicateDepsSet) =
    findDuplicatedDeps(
      ~fetchDepsSubset,
      ~solution,
      (PackageId.Set.empty /* seen */, Package.Set.empty /* duplicated */),
      duplicatesQueue,
    );
  let duplicateDeps = Package.Set.elements(duplicateDepsSet);
  if (List.length(duplicateDeps) == 0) {
    print_endline(">>>>>>>>>E mpty");
  };
  let queue = Queue.create();
  Queue.add((root, nodeModulesPath), queue);
  print_endline(Format.asprintf("Enqueue %a \n---- ", Package.pp, root));
  let* () =
    duplicateDeps
    |> List.map(~f=pkg => {
         let* () =
           RunAsync.ofLwt @@
           Esy_logs_lwt.debug(m =>
             m("NodeModuleLinker: hoisting %a", Package.pp, pkg)
           );
         print_endline(
           Format.asprintf(
             "Enqueue (and later link) %a \n---- ",
             Package.pp,
             pkg,
           ),
         );
         Queue.add((pkg, nodeModulesPath), queue);
         installPkg(~installation, ~nodeModulesPath, pkg);
       })
    |> RunAsync.List.waitAll;
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
