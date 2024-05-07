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
module HoistedGraph = {
  // Note about why K.t is map isn't parameterised on both k and v
  // https://stackoverflow.com/questions/14629642/ocaml-polymorphic-function-on-maps-with-generic-keys
  module type S = {
    // [data] represents variable, one of which is Package.t
    // parameterising over Package.t with [data] is how we keep it out of our abstractions
    type data;
    module Map: Map.S with type key = data; // These maps btw contain keys that represent node module entries. Packages with same name are equal (even if different versions)
    type t = Map.t(node) // likely a map of root nodes
    and node = {
      parent: node,
      data,
      children: Map.t(node),
    };
    let roots: t => Map.t(node);
    let ofRoots: Map.t(node) => t;
    let nodeUpdateChildren: (data, node, node) => node;
    let nodeData: node => data;
    let children: node => Map.t(node);
  };
};

module type S = {
  type data;
  /** Represents the hoisted node_module graph. Each node's id is the package name (not version) */
  type hoistedGraph;
  /** Represents the entry/node in hoisted node_module. See [hoistedGraph('a)] for more details */
  type hoistedGraphNode;

  /** Takes a named list [hypotheticalLineage] of 'a, a [hoistedGraph('a)] named [hoistedGraph], and a node which has a type which can be [hoistedGraph]'s node (but not necessarily so), and returns if it can become a node indeed */
  let hoist:
    (
      ~hypotheticalLineage: list(data),
      ~hoistedGraph: hoistedGraph,
      hoistedGraphNode
    ) =>
    result(hoistedGraph, string);
};

module Make =
       (K: Map.OrderedType, HoistedGraph: HoistedGraph.S with type data = K.t)

         : (
           S with
             type hoistedGraph = HoistedGraph.t and
             type hoistedGraphNode = HoistedGraph.node and
             type data = K.t
       ) => {
  type data = K.t;
  type hoistedGraph = HoistedGraph.t;
  type hoistedGraphNode = HoistedGraph.node;

  let rec proceedMatching = (root, restOfLineage, node) => {
    let children = HoistedGraph.children(root);
    switch (restOfLineage) {
    | [] =>
      let dataField = HoistedGraph.nodeData(root);
      switch (HoistedGraph.Map.find_opt(dataField, children)) {
      | Some(_) => Error("Cant hoist")
      | None => Ok(HoistedGraph.nodeUpdateChildren(dataField, node, root))
      };
    | [h, ...r] =>
      switch (HoistedGraph.Map.find_opt(h, children)) {
      | Some(child) => proceedMatching(child, r, node)
      | None => Error("Cant hoist")
      }
    };
  };

  let hoist = (~hypotheticalLineage, ~hoistedGraph, node) => {
    let roots = HoistedGraph.roots(hoistedGraph);
    switch (hypotheticalLineage) {
    | [h, ...rest] =>
      switch (HoistedGraph.Map.find_opt(h, roots)) {
      | Some(hoistedGraphRoot) =>
        switch (proceedMatching(hoistedGraphRoot, rest, node)) {
        | Ok(newRoot) =>
          HoistedGraph.Map.update(h, _ => Some(newRoot), roots)
          |> HoistedGraph.ofRoots
          |> Result.return
        | Error(e) => Error(e)
        }
      | None => Error("Cant hois")
      }
    | [] => Error("hypotheticalLineage should not be empty")
    };
  };
};
