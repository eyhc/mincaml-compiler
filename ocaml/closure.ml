open Printf
open Utils

type t =
  | Unit
  | Var of Id.t
  (* Pour les entiers *)
  | Int of int
  | Neg of Id.t
  | Add of Id.t * Id.t
  | Sub of Id.t * Id.t
  (* Pour les floats *)
  | Float of float
  | FNeg of Id.t
  | FAdd of Id.t * Id.t
  | FSub of Id.t * Id.t
  | FMul of Id.t * Id.t
  | FDiv of Id.t * Id.t
  (* Tuples *)
  | Tuple of Id.t list
  (* Arrays *)
  | Array of Id.t * Id.t
  | Get of Id.t * Id.t
  | Put of Id.t * Id.t * Id.t
  (* If égal: opérande gauche * opérande droite * then * else  *)
  | IfEq of Id.t * Id.t * t * t
  (* If inférieur ou égal: opérande gauche * opérande droite * then * else  *)
  | IfLE of Id.t * Id.t * t * t
  (* Déclaration d'une variable: (id, type) * valeur * in *)
  | Let of (Id.t * Type.t) * t * t
  (* Lecture d'un tuple : liste des variables recevant les éléments du tuple * le nom de la variable contenant le tuple * in *)
  | LetTuple of (Id.t * Type.t) list * Id.t * t
  (* Création d'une closure: (id * type)  * label de la fonction * variables libres * next (équivalent à un let) *)
  | MakeCls of (Id.t * Type.t) * Id.t * Id.t list * t
  (* Appel à une fonction via apply_direct: label de la fonction * arguments *)
  | ApplyDir of Id.t * Id.t list
  (* Appel à une closure via apply_closure: nom variable contenant la closure * arguments *)
  | ApplyCls of Id.t * Id.t list
  (* Programme: fonctions du programme * code principal *)
  | Prog of fundef list * t
(* Définition d'une fonction: label * arguments * variables libres * code *)
and fundef = {label: (Id.t * Type.t); args: (Id.t * Type.t) list; frees: (Id.t * Type.t) list; code: t}

(************************
   To string functions
************************)

let rec to_string ?(p: string = "") exp: string =
  let tab = "  " in
  let fundef_to_string (fd: fundef): string =
    sprintf("label: %s\nparameters: %s\nfrees variables: %s\ncode:\n%s\n") 
    (fst fd.label) (if List.length fd.args = 0 then "None" else Syntax.infix_to_string (fun (x, y) -> Id.to_string x) fd.args " ")
    (if List.length fd.frees = 0 then "None" else Syntax.infix_to_string (fun (x, y) -> Id.to_string x) fd.frees " ") 
    (to_string ~p:(p^tab) fd.code)
  in
  match exp with
  | Unit -> "()"
  | Var id -> sprintf("%s%s") p (Id.to_string id)
  | Int i -> string_of_int i
  | Float f -> string_of_float f
  | Neg x -> "-" ^ Id.to_string x
  | Add(a, b) -> sprintf("%s%s + %s") p a b
  | Sub(a, b) -> sprintf("%s%s - %s") p a b
  | FNeg x -> Id.to_string x
  | FAdd(a, b) -> sprintf("%s%s +. %s") p a b
  | FSub(a, b) -> sprintf("%s%s -. %s") p a b
  | FMul(a, b) -> sprintf("%s%s *. %s") p a b
  | FDiv(a, b) -> sprintf("%s%s /. %s") p a b
  | Tuple(vars) -> sprintf("(%s)") (Syntax.infix_to_string Id.to_string vars ", ")
  | Array(a, b) -> sprintf("Array.create %s %s") a b
  | Get(a, b) -> sprintf("%s.(%s)") a b
  | Put(a, b , c) -> sprintf("%s.(%s) <- %s") a b c
  | IfEq(a, b, th, els) -> sprintf("%sif %s = %s then\n%s\n%selse\n%s") p a b (to_string ~p:(p^tab) th) p (to_string ~p:(p^tab) els)
  | IfLE(a, b, th, els) -> sprintf("%sif %s <= %s then\n%s\n%selse\n%s") p a b (to_string ~p:(p^tab) th) p (to_string ~p:(p^tab) els)
  | Let((id, t), value, next) -> sprintf("%slet %s = %s in\n%s") p id (to_string value) (to_string ~p:p next)
  | LetTuple(vars, tuple, next) -> sprintf("%slet %s = %s in\n%s") p (Syntax.infix_to_string (fun (x, y) -> Id.to_string x) vars ", ") tuple (to_string ~p:p next)
  | MakeCls((id, t), label, args, next) -> sprintf("%slet %s = make_closure(%s) in\n%s") p 
  id (Syntax.infix_to_string Id.to_string (label :: args) ", ") (to_string ~p:p next)
  | ApplyDir(label, args) -> sprintf("%sapply_direct(%s)") p (Syntax.infix_to_string Id.to_string (label :: args) ", ")
  | ApplyCls(name, args) -> sprintf("%sapply_closure(%s)") p (Syntax.infix_to_string Id.to_string (name :: args) ", ")
  | Prog(funs, main) -> sprintf("%s\nMAIN:\n%s") (Syntax.infix_to_string fundef_to_string funs "\n") (to_string main)

(************************
   Closure conversion functions
************************)

(* 
  Génère un label pour une fonction
  Paramètres:
  - var -> le nom de la fonction
  Retourne: le label de la fonction
*)
let genlabel (var: Id.t): Id.t = "_"^var

(* 
  Cherche l'ensemble des variables dans une expression
  Paramètres:
  - env -> l'environnement de l'expression, un ensemble de variables
  - parent -> le nom de la fonction dans laquelle se trouve cette expression et l'ensemble de ses variables libres
  - exp -> une expression de type knorm_t
  Retourne: une liste de variables
*)
let rec fun_free_vars (env: VarSet.t) (parent: Id.t * VarSet.t) (exp: Knorm.knorm_t): VarSet.t =  
  let find_id_type (id: Id.t): (Id.t * Type.t) =
    let elems = VarSet.elements env in
    List.find (fun (x,y) -> x = id) elems
  in
  match exp with
  | Var(id) when List.exists (fun (x, y) -> x = id) (VarSet.elements env) -> VarSet.singleton (find_id_type id)
  | Add(a, b) | Sub(a, b) | FAdd(a, b) | FSub(a, b) | FMul(a, b) | FDiv(a, b) | Array(a, b) | Get(a, b) ->
      let set = VarSet.singleton (find_id_type a) in
      VarSet.add (find_id_type b) set
  | Tuple(vars) -> VarSet.of_list (List.map (fun x -> find_id_type x) vars)
  | Put(a, b, c)  -> VarSet.of_list (List.map find_id_type [a; b; c])
  | IfEq((a, b), th, els) | IfLE((a, b), th, els) ->
      let a' = find_id_type a in
      let b' = find_id_type b in
      let env' = VarSet.add a' (VarSet.add b' env) in
      let set = VarSet.add a' (VarSet.add b' (fun_free_vars env' parent th)) in
     VarSet.union set (fun_free_vars env' parent els)
  | Let((id, t), value, next) -> 
      let env' = VarSet.add (id, t) env in
      VarSet.union (fun_free_vars env' parent value) (VarSet.remove (id, t) (fun_free_vars env' parent next))
  | LetRec(fd, next) -> 
      let env' = VarSet.union (VarSet.of_list fd.args) env in
      let set = VarSet.union (fun_free_vars env' parent fd.body) (fun_free_vars env parent next) in
      VarSet.diff set (VarSet.of_list fd.args)
  | LetTuple(vars, tuple, next) -> 
      let env' = VarSet.union (VarSet.of_list vars) env in
      VarSet.diff (VarSet.add (find_id_type tuple) (fun_free_vars env' parent next)) (VarSet.of_list vars)
  | App(name, args) -> VarSet.union (VarSet.of_list (List.map (fun x -> find_id_type x) args))
      (if name == fst parent then snd parent else VarSet.empty)
  | _ -> VarSet.empty

(* 
  Convertit le programme et applique la closure conversion dessus
  Paramètres:
  - exp -> une expression de type knorm_t
  Retourne: une liste de fundef (les fonctions) et une expression de type t (le code principal)
*)
let convert (exp: Knorm.knorm_t): fundef list * t =
  let funs = ref [] in
  (* Convertit les noms des fonctions dans une liste d'arguments pour qu'ils soient des labels de fonction *)
  let convert_args (args: Id.t list): Id.t list =
    List.map (fun x -> let label = genlabel x in if List.exists (fun y -> (fst y.label) = label) !funs then label else x) args
  in
  (* 
    Fonction utilitaire pour convert
    Paramètres:
    - parent (optionnel): la fonction dans laquelle se trouve l'expression
    - env: l'environnemnt de l'expression, un ensemble de variables
    - e: l'expression à convertir
    Retourne: l'expression e convertit en type t avec la closure conversion appliquée dessus
  *)
  let rec worker ?(parent: Id.t * VarSet.t = ("", VarSet.empty)) (env: VarSet.t) (e: Knorm.knorm_t): t =
    let get_type (id: Id.t): Type.t =
      snd (List.find (fun (x, y) -> x = id) (VarSet.elements env))
    in
    match e with
    | Unit -> Unit
    | Var(id) when List.exists (fun x -> (genlabel id) = (fst x.label)) !funs ->
        let label = genlabel id in
        let f = List.find (fun x -> label = (fst x.label)) !funs in
        let new_id = Id.genid () in
        MakeCls((new_id, snd f.label), label, List.map fst f.frees, Var(new_id))
    | Var(id) -> Var(id)
    | Int i -> Int(i)
    | Float f -> Float(f)
    | Neg x -> Neg(x)
    | Add(a, b) -> Add(a, b)
    | Sub(a, b) -> Sub(a, b)
    | FNeg x -> FNeg(x)
    | FAdd(a, b) -> FAdd(a, b)
    | FSub(a, b) -> FSub(a, b)
    | FMul(a, b) -> FMul(a, b)
    | FDiv(a, b) -> FDiv(a, b)
    | Get(a, b) -> Get(a, b)
    | Put(a, b, c) -> Put(a, b, c)
    | IfEq((a, b), th, els) -> 
        IfEq(a, b, worker env th, worker env els)
    | IfLE((a, b), th, els) -> 
        IfLE(a, b, worker env th, worker env els)
    | Let((id, t), value, next) -> 
      let env' = VarSet.add (id, t) env in
      (match value with
      (* Les let qui contiennent le résultat de l'appel d'une fonction *)
      | App(name, args) when List.exists (fun x -> (fst x.label) = genlabel name) !funs ->
          let f = List.find (fun x -> (fst x.label) = (genlabel name)) !funs in
          if List.length f.frees > 0 then
            let new_id = Id.genid () in
            MakeCls((new_id, t), genlabel name, List.map fst f.frees, 
            Let((id, snd f.label), ApplyCls(new_id, convert_args args), worker env' next))
          else
            Let((id, t), ApplyDir(genlabel name, convert_args args), worker env' next)
      | Tuple(vars) -> Let((id, t), Tuple(vars), worker env' next)
      | Array(a, b) -> Let((id, t), Array(a, b), worker env' next)
      | _ -> Let((id, t), worker env value, worker env' next))
    | LetRec(fd, next) ->
      (* Ajout des paramètres de la fonction à l'environnement *)
      let env' = VarSet.union (VarSet.of_list fd.args) env in
      let label = genlabel (fst fd.name) in
      let frees = VarSet.diff (fun_free_vars env' parent fd.body) (VarSet.of_list fd.args) in
      (* Ajout temporaire de la fonction dans la liste des fonctions *)
      funs := {label= (label, (snd fd.name)); args= fd.args; frees= VarSet.elements frees; code= Unit} :: !funs;
      (* Conversion du corps de la fonction *)
      let body' = worker ~parent:(fst fd.name, frees) env' fd.body in
      (* Ajout correct de la fonction dans la liste des fonctions *)
      funs := List.filter (fun x -> (fst x.label) <> label) !funs;
      funs := {label= (label, (snd fd.name)); args= fd.args; frees= VarSet.elements frees; code= body'} :: !funs;
      (* Conversion du code après la définition de la fonction *)
      worker env next
    | LetTuple(vars, tuple, next) -> 
      let env' = VarSet.union (VarSet.of_list vars) env in
      LetTuple(vars, tuple, worker env' next)
    (* Appel d'une fonction *)
    | App(name, args) when List.exists (fun x -> (fst x.label) = (genlabel name)) !funs ->
        let f = List.find (fun x -> (fst x.label) = (genlabel name)) !funs in
        if List.length f.frees > 0 then
          (* Fonction avec au moins une variable libre *)
          let id = Id.genid () in
          MakeCls((id, Type.Unit), genlabel name, List.map fst f.frees, ApplyCls(id, convert_args args))
        else
          (* Fonction sans variables libres *)
          ApplyDir(genlabel name, convert_args args)
    (* Appel d'une fonction prédéfinie (print_int par exemple) *)
    | App(name, args) when Typechecker.is_prefef_fun name -> ApplyDir(name, convert_args args)
    (* Appel d'une fonction dans une closure *)
    | App(name, args) -> ApplyCls(name, convert_args args)
    (* On place les déclarations de tableau dans des variables pour simplifier la génération de l'ASML *)
    | Array(a, b) ->
      let id = Id.genid () in
      Let((id, get_type b), Array(a, b), Var(id))
    (* On place les déclarations de tuples dans des variables pour simplifier la génération de l'ASML *)
    | Tuple(vars) ->
        let id = Id.genid () in
        let ts = List.map get_type vars in
        Let((id, Type.Tuple(ts)), Tuple(vars), Var(id))
  (*** Code de convert ***)
  in (!funs, worker VarSet.empty exp)

(* 
  Réalise la closure conversion
  Paramètres:
  - ast -> l'ast du programme
  Retourne: l'ast du programme avec la closure conversion appliquée dessus
*)
let closure (ast: Knorm.knorm_t): t =
  let funs, main = convert ast in
  Prog(funs, main)