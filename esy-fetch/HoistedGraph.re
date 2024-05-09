open RunAsync.Syntax;

type data = NodeModule.t;
module Map = Map.Make(NodeModule);
type t = Map.t(node)
and node = {
  parent: option(Lazy.t(node)),
  data,
  children: Lazy.t(Map.t(node)),
};

let rec parentPp = (fmt, parentNode) => {
  switch (parentNode) {
  | Some(parentNode) =>
    let parentNode = Lazy.force(parentNode);
    NodeModule.pp(fmt, parentNode.data);
  | None => Fmt.any("<no-parent>", fmt, ())
  };
}
and childPp = NodeModule.pp
and childrenPp = (fmt, children) => {
  let sep = fmt => Fmt.any(" -- ", fmt);
  let childrenAsList =
    children
    |> Lazy.force
    |> Map.bindings
    |> List.map(~f=((child, _true)) => child);
  if (List.length(childrenAsList) == 0) {
    Fmt.any("<no-children>", fmt, ());
  } else {
    childrenAsList |> Fmt.list(~sep, childPp, fmt);
  };
}
and nodePp = (fmt, node) => {
  let {parent, data, children} = node;
  Fmt.pf(
    fmt,
    "\ndata: %a\nParents: %a\nChildren: %a",
    NodeModule.pp,
    data,
    parentPp,
    parent,
    childrenPp,
    children,
  );
};

let roots = roots => roots;
let empty = Map.empty;
let ofRoots = roots => roots;
let nodeUpdateChildren = (dataField, newNode, parent) => {
  {
    ...parent,
    children: lazy(Map.add(dataField, newNode, Lazy.force(parent.children))),
  };
};
let nodeData = node => node.data;

let makeCache: Hashtbl.t(data, node) = Hashtbl.create(100);
let rec makeNode' = (~traverse, ~parent, ~data) => {
  let init = Map.empty;
  let f = (acc, child) => {
    Map.add(
      child,
      makeNode(
        ~traverse,
        ~parent=Some(lazy(makeNode(~traverse, ~parent, ~data))),
        ~data=child,
      ),
      acc,
    );
  };
  {parent, data, children: lazy(List.fold_left(~f, ~init, traverse(data)))};
}
and makeNode:
  (
    ~traverse: NodeModule.t => list(NodeModule.t),
    ~parent: option(Lazy.t(node)),
    ~data: data
  ) =>
  node =
  (~traverse, ~parent, ~data) => {
    switch (Hashtbl.find_opt(makeCache, data)) {
    | Some(node) => node
    | None => makeNode'(~traverse, ~parent, ~data)
    };
  };
let addRoot = (~node, graph) => {
  Map.add(node.data, node, graph);
};
let walk = (~f: node => RunAsync.t(unit), graph: t): RunAsync.t(unit) => {
  let roots = roots(graph);
  Map.fold(
    (_k, v, acc) => {
      let* () = acc;
      f(v);
    },
    roots,
    RunAsync.return(),
  );
};

let constructLineage' = (acc, parent) => {
  switch (parent.parent) {
  | Some(grandparent) => [Lazy.force(grandparent), ...acc]
  | None => acc
  };
};
let constructLineage = constructLineage'([]);

let rec nodeModulesPathFromParent = (~baseNodeModulesPath, parent) => {
  switch (parent.parent) {
  | Some(_grandparent) =>
    let lineage = constructLineage(parent); // This lineage is a list starting from oldest ancestor
    let lineage = List.tl(lineage); // skip root which is just parent id
    let init = baseNodeModulesPath;
    let f = (acc, node) => {
      Path.(acc / NodeModule.name(node.data) / "node_modules");
    };
    List.fold_left(lineage, ~f, ~init);
  | None => /* most likely case. ie all pkgs are directly under root  */ baseNodeModulesPath
  };
};
