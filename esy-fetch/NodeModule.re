open EsyPackageConfig;
type t = Solution.pkg;
let compare = (a, b) => Package.(String.compare(a.name, b.name));
module Id = {
  type nonrec t = t;
  let compare = compare;
};
module Map = Map.Make(Id);
module Set = Set.Make(Id);
module SolutionGraph = {
  type node = {
    parents: list(Solution.pkg),
    data: Solution.pkg,
    children: Map.t(bool),
  };
  type state = {
    queue: Queue.t(node),
    visited: PackageId.Map.t(bool),
  };
  type traversalFn = Solution.pkg => list(Solution.pkg);
  let createChildrenMap = (~traverse, node) => {
    let f = (acc, child) => {
      Map.add(child, true, acc);
    };
    node |> traverse |> List.fold_left(~f, ~init=Map.empty);
  };
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
    let visited =
      PackageId.Map.empty |> PackageId.Map.add(root.Package.id, true);
    {queue, visited};
  };
  let take = (~traverse, iterable) => {
    let {queue, visited} = iterable;
    let f = node => {
      let {parents, data: pkg, children} = node;
      let f = ((childNode, _true)) => {
        Queue.push(
          {
            parents: parents @ [pkg],
            data: childNode,
            children: createChildrenMap(~traverse, childNode),
          },
          queue,
        );
      };
      List.iter(~f, Map.bindings(children));
      node;
    };
    let noneIfVisited = (visited, node) =>
      isVisited(visited, node.data) ? None : Some(node);
    queue
    |> Queue.take_opt
    |> Option.bind(~f=noneIfVisited(visited))
    |> Option.map(~f);
  };
};
