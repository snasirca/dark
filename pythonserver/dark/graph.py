
def get_all_graphnames() -> List[str]:
  return [f[:-5] for f in os.listdir("appdata") if f.endswith(".dark")]

def filename_for(name:str) -> str:
  return "appdata/" + name + ".dark"

class Graph:

  def __getattr__(self, name:str) -> Any:
    # dynamically build useful constructs
    if name == "reverse_edges":
      result : Dict[str, List[str]] = {}
      for k in self.nodes.keys():
        result[k] = []
      for s,ts in self.edges.items():
        for (t,_) in ts:
          result[t].append(s)
      return result

    if name == "pages":
      return {k: v for k,v in self.nodes.items() if v.is_page()}

    if name == "datastores":
      return {k: v for k,v in self.nodes.items() if isinstance(v, datastore.Datastore)}


  def _add_datastore_field(self, id:ID, fieldname:str, typename:str, is_list:bool) -> None:
    ds = self.datastores[id]
    fieldFn = getattr(fields, typename, None)
    if fieldFn:
      ds.add_field(fieldFn(fieldname, is_list=is_list))
    else:
      ds.add_field(fields.Foreign(fieldname, typename, is_list=is_list))

  def _add_edge(self, src_id:ID, target_id:ID, param:str) -> None:
    src = self.nodes[src_id]
    target = self.nodes[target_id]

    # Can't have two edges to the same target
    if self.get_parents(target).get(param) != None:
      # TODO: exception for datasinks like DBs and APIs
      raise Exception("There's already an edge here")

    # check the types at both ends of the edge are compatible
    src_type = src.get_return_type()
    target_type = target.get_parameter_type(param)
    try:
      types.check(src_type, target_type)
    except types.DTypeError as e:
      raise Exception("Can't turn a %s into a %s (%s -> %s)" % (e.p1, e.p2, src.name(), target.name()))


    self.edges[src.id()].append((target.id(), param))

  #############
  # execution
  #############
  def get_children(self, node:Node) -> Dict[str, Node]:
    children = self.edges[node.id()] or []
    return {param: self.nodes[c] for (c, param) in children}

  def get_parents(self, node:Node) -> Dict[str, Node]:
    parents = self.reverse_edges.get(node.id(), [])
    result : List[Tuple[str, Node]] = []
    for p in parents:
      t = self.nodes[p]
      paramname = self.get_target_param_name(node, t)
      result += [(paramname, t)]
    return {paramname: t for (paramname, t) in result}

  def get_named_parents(self, node:Node, paramname:str) -> List[Node]:
    return [v for k,v in self.get_parents(node).items()
            if paramname == k]

  def get_target_param_name(self, target:Node, src:Node) -> str:
    for (c, p) in self.edges[src.id()]:
      if c == target.id():
        return p
    assert(False and "Shouldnt happen")

  def execute(self, node:Node, only:Node = None, eager:Dict[Node, Any] = {}) -> Any:
    # debug("executing node: %s, with only=%s and eager=%s" % (node, only, eager))
    if node in eager:
      result = eager[node]
    else:
      args = {}
      for paramname, p in self.get_parents(node).items():
        # make sure we don't traverse beyond datasinks (see find_sink_edges)
        if only in [None, p]:
          # make sure we don't traverse beyond datasources
          new_only = p if p.is_datasource() else None

          args[paramname] = self.execute(p, eager=eager, only=new_only)

      result = node.exe(**args)

    return pyr.freeze(result)

  def find_sink_edges(self, node:Node) -> Set[Tuple[Node, Node]]:
    # debug("finding sink edges: %s" % (node))
    results : Set[Tuple[Node, Node]] = set()
    for _, c in self.get_children(node).items():
      if c.is_datasink():
        results |= {(node, c)}
      else:
        results |= self.find_sink_edges(c)
    return results

  def run_input(self, node:Node, val:Any) -> None:
    # debug("running input: %s (%s)" % (node, val))
    for (parent, sink) in self.find_sink_edges(node):
      # debug("run_input on sink,parent: %s, %s" %(sink, parent))
      self.execute(sink, only=parent, eager={node: val})

  def run_output(self, node:Node) -> Any:
    # print("run_output on node: %s" % (node))
    return self.execute(node)


  def to_debug(self, cursor:Optional[ID]) -> str:
    result = []
    edges = self.to_frontend_edges()
    for e in edges:
      out = "%s --(%s)--> %s)" % (e["source"], e["paramname"], e["target"])
      result.append(out)
    for n in self.nodes.values():
      if n.id() in self.edges and \
         len(self.edges[n.id()]) == 0 and \
         len(self.reverse_edges[n.id()]) == 0:
        result.append("Solo node: " + n.id())

    return "\n".join(sorted(result))
