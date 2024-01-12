module IdSet = Set.Make(struct type t = Id.t let compare = compare end)

module VarSet = Set.Make(struct type t = (Id.t * Type.t) let compare = compare end)
