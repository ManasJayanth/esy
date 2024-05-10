open RunAsync.Syntax;
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
// Note about why K.t is map isn't parameterised on both k and v
// https://stackoverflow.com/questions/14629642/ocaml-polymorphic-function-on-maps-with-generic-keys
module type S = {
  // [data] represents variable, one of which is Package.t
  // parameterising over Package.t with [data] is how we keep it out of our abstractions
  type data;
  module Map: Map.S with type key = data; // These maps btw contain keys that represent node module entries. Packages with same name are equal (even if different versions)
  type t = Map.t(node) // likely a map of root nodes
  and node = {
    parent: option(Lazy.t(node)),
    data,
    children: Lazy.t(Map.t(node)),
  };
  let parent: node => option(Lazy.t(node));
  let roots: t => Map.t(node);
  let ofRoots: Map.t(node) => t;
  let nodeUpdateChildren: (data, node, node) => node;
  let nodeData: node => data;
  let makeNode: (~parent: option(Lazy.t(node)), ~data: data) => node;
  let nodePp: Fmt.t(node);
  let dataListPp: Fmt.t(list(data));
  let addRoot: (~node: node, t) => t;
};

type data = NodeModule.t;
let traversalFn = ref(_ => []);
module Map = Map.Make(NodeModule);
type t = Map.t(node)
and node = {
  parent: option(Lazy.t(node)),
  data,
  children: Lazy.t(Map.t(node)),
};

let dataListPp = (fmt, data) => {
  let sep = fmt => Fmt.any(" -- ", fmt);
  if (List.length(data) == 0) {
    Fmt.any("<empty>", fmt, ());
  } else {
    data |> Fmt.list(~sep, Package.pp, fmt);
  };
};

let parent = ({parent, _}) => parent;
let rec parentPp = (fmt, parentNode) => {
  switch (parentNode) {
  | Some(parentNode) =>
    let parentNode = Lazy.force(parentNode);
    NodeModule.pp(fmt, parentNode.data);
  | None => Fmt.any("<no-parent>", fmt, ())
  };
}
and parentsPp = fmt => {
  let sep = fmt => Fmt.any(" -> ", fmt);
  Fmt.list(~sep, parentPp, fmt);
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
    "\ndata: %a\nParent: %a\nChildren: %a",
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
  let newNode = {...newNode, parent: Some(lazy(parent))};
  {
    ...parent,
    children: lazy(Map.add(dataField, newNode, Lazy.force(parent.children))),
  };
};
let nodeData = node => node.data;

let makeCache: Hashtbl.t(data, node) = Hashtbl.create(100);
let rec makeNode' = (~parent, ~data) => {
  let init = Map.empty;
  let f = (acc, child) => {
    Map.add(
      child,
      makeNode(~parent=Some(lazy(makeNode(~parent, ~data))), ~data=child),
      acc,
    );
  };
  {
    parent,
    data,
    children: lazy(List.fold_left(~f, ~init, traversalFn^(data))),
  };
}
and makeNode: (~parent: option(Lazy.t(node)), ~data: data) => node =
  (~parent, ~data) => {
    switch (Hashtbl.find_opt(makeCache, data)) {
    | Some(node) => node
    | None => makeNode'(~parent, ~data)
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

let init = (~traverse: 'a => list('a)) => {
  traversalFn := traverse;
  empty;
};

/* let rec makeHoistedGraph = (~data, ~revLineage) => { */
/*   let parent = */
/*     switch (revLineage) { */
/*     | [] => None */
/*     | [single] => */
/*       Some(lazy(HoistedGraph.makeNode(~data=single, ~parent=None))) */
/*     | [h, ...rest] => */
/*       Some(lazy(makeHoistedGraph(~data=h, ~revLineage=rest))) */
/*     }; */
/*   HoistedGraph.makeNode(~data, ~parent); */
/* }; */
