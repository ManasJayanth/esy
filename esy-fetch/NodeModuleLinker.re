/* open EsyPackageConfig; */
/* open RunAsync.Syntax; */

/* /\** */
/*    Makes sure we dont link opam packages in node_modules */
/*  *\/ */
/* let getNPMChildren = (~solution, ~fetchDepsSubset, ~node) => { */
/*   let f = (pkg: Solution.pkg) => { */
/*     switch (pkg.Package.version) { */
/*     | Opam(_) => false */
/*     | Npm(_) */
/*     | Source(_) => true */
/*     /\* */
/*         Allowing sources here would let us resolve to github urls for */
/*         npm dependencies. Atleast in theory. TODO: test this */
/*      *\/ */
/*     }; */
/*   }; */
/*   Solution.dependenciesBySpec(solution, fetchDepsSubset, node) */
/*   |> List.filter(~f); */
/* }; */

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
/* module NodeModule = { */
/*   /\** */
/*      These modules represent an entry (a package) in node_modules folder. */
/*      packages in the node_modules folder have be unique by name. JS */
/*      packages in the graph could have more one versions present - we */
/*      have to carefully avoid conflicts and save disk space. */
/*    *\/ */
/*   type t = Package.t; */
/*   let compare = (a, b) => { */
/*     Package.(String.compare(a.name, b.name)); */
/*   }; */
/*   module Set = */
/*     Set.Make({ */
/*       type nonrec t = t; */
/*       let compare = compare; */
/*     }); */
/*   module Graph = { */
/*     type node = { */
/*       data: t, */
/*       children: list(node), */
/*     }; */
/*   }; */
/* }; */
/* let rec loop = (~solution, ~fetchDepsSubset, ~pkgQueue, ~hoistedGraphRoots) => { */
/*   switch (Queue.take_opt(pkgQueue)) { */
/*   | Some(pkg) => hoistedGraphRoots */
/*   | None => hoistedGraphRoots */
/*   }; */
/* }; */
/* let link = (~installation, ~solution, ~projectPath, ~fetchDepsSubset) => { */
/*   let root = Solution.root(solution); */
/*   let pkgQueue = Queue.create(); */
/*   Queue.add(root, pkgQueue); */
/*   let hoistedGraphRoots = */
/*     loop( */
/*       ~solution, */
/*       ~fetchDepsSubset, */
/*       ~pkgQueue, */
/*       ~hoistedGraphRoots=Package.Set.empty, */
/*     ); */
/*   ignore(hoistedGraphRoots); */
/*   RunAsync.return(); */
/* }; */

let link = () => RunAsync.return();
