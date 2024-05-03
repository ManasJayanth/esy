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

/**
   Checks if [nodeModuleEntry] can be hoisted under the last element
   in [hypotheticalLineage].

   Notes: Why an entire queue of nodes and not just [head] (from the
   caller function, [hoistLineage]) which gets pushed as last element
   in [hypotheticalLineage]. We have to append to [head] after all,
   isn't it?

   Because, otherwise who will we efficiently find [head] in
   [hoistedGraph]. Having a complete lineage enables us to iterate
   over it and index [head] quickly.

*/

let isHoistableTo = (~hypotheticalLineage, ~hoistedGraph, pkg) => {

  // First compare the root
  let roots = HoistedGraph.roots(hoistedGraph);
  switch(Queue.
  Package.Map.find_opt(
  let f = (hoistedGraphNodeAcc, parent) => {
    switch(HoistedGraph.lookupChild(parent.data, hoistedGraphNodeAcc)) {
    | Some(_hoistedGraphNodeChild) => false
      | None => true
  };
  Queue.fold(f, init, hypotheticalLineage);
};

let hoist = (~node, ~lineage, ~graph) => {
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
  let rec aux = (~hypotheticalLineage, ~lineage, pkg) => {
    switch (lineage) {
    | [head, ...rest] =>
      Queue.push(head, hypotheticalLineage);
    // TODO: get rid of queue. We end up traversing right after
    // creating it.
      //
      if (isHoistableTo(~hypotheticalLineage, ~hoistedGraph, pkg)) {
        hypotheticalLineage |> Queue.to_seq |> List.of_seq;
      } else {
        aux(~hypotheticalLineage, ~lineage=rest, pkg);
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
      let nodeModuleEntry = Graph.ofSolutionGraphNode(node);
      let hoistedLineage =
        hoistLineage(~lineage=List.rev(parents), ~hoistedGraph, node);
      let hoistedGraph =
        hoist(
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
  let traverse = getNPMChildren(~fetchDepsSubset, ~solution);
  let hoistedGraph = HoistedGraph.empty;
  solution
  |> SolutionGraph.iterator
  |> iterateSolution(~traverse, ~hoistedGraph);
};
