/* open EsyPackageConfig; */
/* open RunAsync.Syntax; */

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

let isHoistableTo = _ => true;
let hoist = (~node, ~shortenedLineage) =>
  if (List.length(shortenedLineage) > 0) {
    NodeModule.(
      print_endline(
        Format.asprintf(
          "Node %a will be hoisted to %a",
          Package.pp,
          node.SolutionGraph.data,
          SolutionGraph.parentsPp,
          shortenedLineage,
        ),
      )
    );
  } else {
    print_endline(
      Format.asprintf(
        "Node %a will be hoisted to <root>",
        Package.pp,
        node.NodeModule.SolutionGraph.data,
      ),
    );
  };

let rec iterateParents = (parentsSoFar, lineage) =>
  if (isHoistableTo(parentsSoFar)) {
    parentsSoFar;
  } else {
    switch (lineage) {
    | [head, ...rest] => iterateParents(parentsSoFar @ [head], rest)
    | [] => failwith("Cannot hoist")
    };
  };

let rec iterateSolution = (~traverse, iterableSolution) => {
  NodeModule.(
    switch (SolutionGraph.take(~traverse, iterableSolution)) {
    | Some((node, nextIterable)) =>
      let SolutionGraph.{parents, _} = node;
      let shortenedLineage = iterateParents([], parents);
      hoist(~node, ~shortenedLineage);
      iterateSolution(~traverse, nextIterable);
    | None => RunAsync.return()
    }
  );
};
let link = (~fetchDepsSubset, ~solution) => {
  let traverse = getNPMChildren(~fetchDepsSubset, ~solution);
  solution |> NodeModule.SolutionGraph.iterator |> iterateSolution(~traverse);
};
