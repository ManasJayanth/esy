open EsyPackageConfig;
type t = Solution.pkg;
let compare = (a, b) => Package.(String.compare(a.name, b.name));
module Id = {
  type nonrec t = t;
  let compare = compare;
};
module Map = Map.Make(Id);
module Set = Set.Make(Id);

let createChildrenMap = (~traverse, node) => {
  open Package;
  let f = (acc, child) => {
    Map.add(child, true, acc);
  };
  node |> traverse |> List.fold_left(~f, ~init=Map.empty);
};

module SolutionGraph = {
  type parents = list(node)
  and node = {
    parents,
    data: Solution.pkg,
    children: Package.Map.t(bool),
  };
  let rec parentPp = (fmt, parentNode) => Package.pp(fmt, parentNode.data)
  and parentsPp = fmt => {
    let sep = fmt => Fmt.any(" -> ", fmt);
    Fmt.list(~sep, parentPp, fmt);
  }
  and childPp = Package.pp
  and childrenPp = (fmt, children) => {
    let sep = fmt => Fmt.any(" -- ", fmt);
    children
    |> Package.Map.bindings
    |> List.map(~f=((child, _true)) => child)
    |> Fmt.list(~sep, childPp, fmt);
  }
  and nodePp = (fmt, node) => {
    let {parents, data, children} = node;
    Fmt.pf(
      fmt,
      "data: %a\n%a\n%a",
      Package.pp,
      data,
      parentsPp,
      parents,
      childrenPp,
      children,
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
  let iterator = (~traverse, solution) => {
    let queue = Queue.create();
    let root = Solution.root(solution);
    let children = createChildrenMap(~traverse, root);
    Queue.push({data: root, parents: [], children}, queue);
    let visited = PackageId.Map.empty;
    {queue, visited};
  };
  let take = (~traverse, iterable) => {
    let {queue, visited} = iterable;
    let dequeue = node => {
      let {parents, data: pkg, children} = node;
      let f = ((childNode, _true)) =>
        if (!isVisited(visited, childNode)) {
          Queue.push(
            {
              parents: parents @ [node],
              data: childNode,
              children: createChildrenMap(~traverse, childNode),
            },
            queue,
          );
        };
      List.iter(~f, Package.Map.bindings(children));
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

    solution |> iterator(~traverse) |> loop;
  };
};
