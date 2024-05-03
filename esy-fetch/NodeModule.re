open EsyPackageConfig;
type t = Solution.pkg;
let compare = (a, b) => Package.(String.compare(a.name, b.name));
module Id = {
  type nonrec t = t;
  let compare = compare;
};
module Map = Map.Make(Id);
module Set = Set.Make(Id);

/* let createChildrenMap = (~traverse, node) => { */
/*   open Package; */
/*   let f = (acc, child) => { */
/*     Map.add(child, true, acc); */
/*   }; */
/*   node |> traverse |> List.fold_left(~f, ~init=Map.empty); */
/* }; */

module SolutionGraph = {
  type parents = list(node)
  and node = {
    parents,
    data: Solution.pkg,
  };
  let rec parentPp = (fmt, parentNode) => Package.pp(fmt, parentNode.data)
  and parentsPp = fmt => {
    let sep = fmt => Fmt.any(" -> ", fmt);
    Fmt.list(~sep, parentPp, fmt);
  }
  /* and childPp = Package.pp */
  /* and childrenPp = (fmt, children) => { */
  /*   let sep = fmt => Fmt.any(" -- ", fmt); */
  /*   let childrenAsList = */
  /*     children */
  /*     |> Package.Map.bindings */
  /*     |> List.map(~f=((child, _true)) => child); */
  /*   if (List.length(childrenAsList) == 0) { */
  /*     Fmt.any("<no-children>", fmt, ()); */
  /*   } else { */
  /*     childrenAsList |> Fmt.list(~sep, childPp, fmt); */
  /*   }; */
  /* } */
  and nodePp = (fmt, node) => {
    let {parents, data} = node;
    Fmt.pf(
      fmt,
      "\ndata: %a\nParents: %a\n",
      Package.pp,
      data,
      parentsPp,
      parents,
    );
  };
  type state = {
    queue: Queue.t(node),
    visited: PackageId.Map.t(bool),
  };
  type traversalFn = Solution.pkg => list(Solution.pkg);
  let isVisited = (visitedMap, node) => {
    visitedMap
    |> PackageId.Map.find_opt(node.Package.id)
    |> Stdlib.Option.value(~default=false);
  };
  let iterator = solution => {
    let queue = Queue.create();
    let root = Solution.root(solution);
    Queue.push({data: root, parents: []}, queue);
    let visited = PackageId.Map.empty;
    {queue, visited};
  };
  let take = (~traverse, iterable) => {
    let {queue, visited} = iterable;
    let dequeue = node => {
      let {parents, data: pkg} = node;
      let f = childNode =>
        if (!isVisited(visited, childNode)) {
          Queue.push({parents: parents @ [node], data: childNode}, queue);
        };
      pkg |> traverse |> List.iter(~f);
      let visited =
        PackageId.Map.update(pkg.Package.id, _ => Some(true), visited);
      (node, {queue, visited});
    };
    queue |> Queue.take_opt |> Option.map(~f=dequeue);
  };
  let debug = (~traverse, solution) => {
    let rec loop = iterableSolution => {
      switch (take(~traverse, iterableSolution)) {
      | Some((node, nextIterSolution)) =>
        print_endline(Format.asprintf("%a", nodePp, node));
        loop(nextIterSolution);
      | None => ()
      };
    };

    solution |> iterator |> loop;
  };
};
