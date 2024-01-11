open Printf

(* Remove duplicates values in list *)
let rec remove_dups list =
  match list with
  | [] -> []
  | [x] -> [x]
  | x :: tail -> if List.exists (fun y -> x = y) tail then remove_dups tail else x :: remove_dups tail


type t =
  | Unit
  | Int of int
  | Var of Id.t
  (* Pour les entiers *)
  | Neg of Id.t
  | Add of Id.t * Id.t
  | Sub of Id.t * Id.t
  (* If égal: (opérande gauche * opérande droite) * then * else  *)
  | IfEq of (Id.t * Id.t) * t * t
  (* If inférieur ou égal: (opérande gauche * opérande droite) * then * else  *)
  | IfLE of (Id.t * Id.t) * t * t
  (* Déclaration d'une variable: nom * valeur * in *)
  | Let of Id.t * t * t
  (* Une fonction: en-tête * code *)
  | LetRec of funheader * t
  (* Création d'une closure: label de la fonction * variables libres *)
  | MakeClosure of Id.t * Id.t list
  (* Appel à une fonction via apply_direct: label de la fonction * arguments *)
  | ApplyDirect of Id.t * Id.t list
  (* Appel à une closure via apply_closure: nom variable contenant la closure * arguments *)
  | ApplyClosure of Id.t * Id.t list
  (* Appel à une fonction prédéfinie: nom de la fonction * arguments *)
  | ApplyPredef of Id.t * Id.t list
  (* Programme: fonctions du programme * code principal *)
  | Prog of t list * t
(* En-tête d'une fonction: label * arguments * variables libres * variables locales * fonction parente (peut être None) *)
and funheader = {label: Id.t; args: Id.t list; frees: Id.t list; locals: Id.t list; parent: funheader ref option;}

(************************
   To string functions
************************)

let rec infix_to_string (to_s : 'a -> string) (l : 'a list) (op : string) : string = 
  match l with 
  | [] -> ""
  | [x] -> to_s x
  | hd :: tl -> (to_s hd) ^ op ^ (infix_to_string to_s tl op)

let rec to_string ?(p: string = "") (exp: t): string =
  let tab = "  " in
  let arithemic_op_to_string (p: string) (a: Id.t) (b: Id.t) (op: string): string =
    sprintf "%s%s %s %s" p (Id.to_string a) op (Id.to_string b)
  in
  let if_to_string (p: string) (a: Id.t) (b: Id.t) (op: string) (th: t) (els: t): string =
    sprintf "%sif %s %s %s then\n%s\n%selse\n%s\n" p a op b (to_string ~p:(p^tab) th) p (to_string ~p:(p^tab) els)
  in
  match exp with
  | Int i -> string_of_int i
  | Var id -> Id.to_string id
  | Neg a -> sprintf "-%s" (Id.to_string a)
  | Add(a, b) -> arithemic_op_to_string p a "+" b
  | Sub(a, b) -> arithemic_op_to_string p a "-" b
  | IfEq((a, b), t, e) -> if_to_string p a b "=" t e
  | IfLE((a, b), t, e) -> if_to_string p a b "<=" t e
  | Let(id, value, next) -> sprintf "%slet %s = %s in\n%s" p (Id.to_string id) (to_string value) (to_string ~p:p next)
  | LetRec(fd, body) -> sprintf("label: %s\nfree variables: %s\nparameters: %s\ncode:\n%s\n") fd.label 
                          (if (List.length fd.frees) = 0 then "None" else infix_to_string Id.to_string fd.frees " ")
                          (if (List.length fd.args) = 0 then "None" else infix_to_string Id.to_string fd.args " ")
                          (to_string ~p:tab body)
  | MakeClosure(label, free_vars) -> sprintf "%smake_closure(%s)" p (infix_to_string Id.to_string (label :: free_vars) ", ")
  | ApplyDirect(label, args) -> sprintf "%sapply_direct(%s)" p (infix_to_string Id.to_string (label :: args) ", ")
  | ApplyClosure(name, args) -> sprintf "%sapply_closure(%s)" p (infix_to_string Id.to_string (name :: args) ", ")
  | ApplyPredef(name, args) -> sprintf "%s%s %s" p name (infix_to_string Id.to_string args " ")
  | Prog(funs, main) -> sprintf "%s\nMain:\n%s" (infix_to_string to_string funs "\n") (to_string main)
  | _ -> ""

(************************
   Closure conversion functions
************************)

let insert_let e k =
  let name = Id.genid () in
  let e' = k (Var(name)) in
  Let(name, e, e')

(* 
  Génère un label pour une fonction
  Paramètres:
  - x -> le nom de la fonction
  Retourne: un label
*)
let gen_label (x: Id.t): Id.t = "fun_"^x

(* 
  Extrait la liste des noms des fonctions du programme
  Paramètres:
  - ast -> l'ast du programme
  Retourne: une liste de Id.t   
*)
let rec find_funcs_names (ast: Knorm.knorm_t): Id.t list =
  match ast with
  | Let((id, t), value, next) -> find_funcs_names next
  | LetRec(fd, next) -> (fst fd.name) :: (find_funcs_names fd.body @ find_funcs_names next)
  | _ -> []

(* 
  Extrait la liste des variables locales d'une fonction
  Paramètres:
  - e -> le code de la fonction
  Retourne: une liste de Id.t   
*)
let rec find_locals_vars (e: Knorm.knorm_t): Id.t list =
  match e with
  | IfEq((a, b), t, els) | IfLE((a, b), t, els) -> find_locals_vars t @ find_locals_vars els
  | Let((id, t), value, next) -> [id] @ find_locals_vars next
  | LetRec(fd, next) -> find_locals_vars next
  | _ -> []

(* 
  Extrait la liste des variables libres d'une fonction
  Paramètres:
  - code -> le code de la fonction
  - args -> les arguments de la fonction
  - locals -> les variables locales de la fonction
  - funcs -> les noms des fonctions du programme
  Retourne: une liste de Id.t
*)
let find_free_vars (code: Knorm.knorm_t) (args: Id.t list) (locals: Id.t list) (funcs: Id.t list): Id.t list =
  let is_free (var: Id.t): bool =
    let f x = x = var in
    Bool.not (List.exists f args || List.exists f locals || List.exists f funcs)
  in
  let rec worker (e: Knorm.knorm_t): Id.t list =
    match e with
    | Var id -> if is_free id then [id] else []
    | Add(a, b) | Sub(a, b) -> (if is_free a then [a] else []) @ (if is_free b then [b] else [])
    | IfEq((a, b), t, els) | IfLE((a, b), t, els) -> (if is_free a then [a] else []) @ (if is_free b then [b] else []) @
                                                      worker t @ worker els
    | Let((id, t), value, next) -> worker value @ worker next
    | LetRec(fd, next) -> worker next
    | App(name, a) -> List.filter_map (fun x -> if is_free x then Some x else None) a
    | _ -> []
  in
  (*** Code de find_free_vars ***)
  worker code

(* 
  Créér les en-têtes des fonctions d'un programme
  Paramètres:
  - ast -> l'ast d'un programme
  Retourne: une liste de (funheader * Knorm.knorm_t)
*)
let rec make_funcs_headers (ast: Knorm.knorm_t) (funcs_names: Id.t list): (funheader * Knorm.knorm_t) list =
  (* 
    Créer l'en-tête d'une fonction et celui des fonctions internes à celle-ci
    Paramètres:
    - fd -> la définition de la fonction
    - parent -> un pointeur sur le header de la fonction parent
    Retourne: une liste de (funheader * Knorm.knorm_t)
  *)
  let rec make_func_header (fd: Knorm.fundef) (parent: funheader ref option): (funheader * Knorm.knorm_t) list =
    (* Les arguments de la fonction *)
    let args = List.map (fun x -> fst x) fd.args in
    (* Les variables locales de la fonction *)
    let locals = find_locals_vars fd.body in
    (* Les variables libres de la fonction *)
    let frees = find_free_vars fd.body args locals funcs_names in
    (* L'en-tête de la fonction *)
    let header = ref {label= gen_label (fst fd.name); args= args; frees= frees; locals= locals; parent= parent} in
    (* 
      Créér les en-têtes des sous-fonctions de la fonction 
      Paramètres:   
      - e -> le code la fonction
      Retourne: une liste de (funheader * Knorm.knorm_t)
    *)
    let rec make_sub_funcs_headers (e: Knorm.knorm_t): (funheader * Knorm.knorm_t) list =
      match e with
      | Let((id, t), value, next) -> make_sub_funcs_headers next
      | LetRec(fd, next) -> make_func_header fd (Some(header)) @ make_sub_funcs_headers next
      | _ -> []
    in
    (*** Code de make_func_header  ***)
    [(!header, fd.body)] @ make_sub_funcs_headers fd.body
  in
  match ast with
    | Let((id, t), value, next) -> make_funcs_headers next funcs_names
    | LetRec(fd, next) -> make_func_header fd None @ make_funcs_headers next funcs_names
    | _ -> []

(* 
  Convertit et applique la closure conversion sur le code d'une fonction
  Paramètres:
  - header -> l'en-tête de la fonction
  - code -> le code de la fonction
  - funcs -> les headers des autres fonctions du programme
  Retourne: le code convertit avec la closure conversion appliquée dessus
*)
let convert_func_body (header: funheader) (code: Knorm.knorm_t) (funcs: funheader list): t =
  let is_func_name (var: Id.t): bool =
    let label = gen_label var in
    List.exists (fun x -> x.label = label) funcs
  in
  let find_header (var: Id.t): funheader =
    let label = gen_label var in
    List.find (fun x -> x.label = label) funcs
  in
  let rename (a: Id.t) (to_rename: (Id.t * Id.t) list): Id.t =
    if List.exists (fun (x,y) -> x = a) to_rename then
      snd (List.find (fun (x,y) -> x = a) to_rename)
    else
      a
  in
  let rec worker ?(to_rename: (Id.t * Id.t) list = []) (e: Knorm.knorm_t): t =
    match e with
    | Int i -> Int(i)
    | Var(id) -> if is_func_name id then
                  let h = find_header id in
                  if List.length h.frees = 0 then
                   insert_let (MakeClosure(gen_label id, [])) (fun x -> x)
                  else
                   insert_let (MakeClosure(gen_label id, h.frees)) (fun x -> x)
                 else 
                  Var(id)
    | Add(a, b) -> Add(rename a to_rename, rename b to_rename)
    | Sub(a, b) -> Sub(rename a to_rename, rename b to_rename)
    | IfEq((a, b), t, els) -> IfEq((rename a to_rename, rename b to_rename), worker t, worker els)
    | IfLE((a, b), t, els) -> IfLE((rename a to_rename, rename b to_rename), worker t, worker els)
    | Let((id, t), App(name, a), next) -> 
      let h = find_header name in
        if (List.length h.frees) = 0 then
          Let(id, ApplyDirect(gen_label name, a), worker next)
        else 
          Let(id, MakeClosure(gen_label name, h.frees), 
          let new_id = Id.genid () in
          Let(new_id, ApplyClosure(id, a), worker ~to_rename:[(id, new_id)] next)
          )
    | Let((id, t), value, next) -> Let(id, worker value, worker next)
    | LetRec(fd, next) -> worker next
    | App(name, a) -> let h = find_header name in
                         if (List.length h.frees) = 0 then
                          ApplyDirect(gen_label name, a)
                         else 
                          insert_let (MakeClosure(gen_label name, h.frees)) (fun x -> match x with | Var id -> ApplyClosure(id, a) | _ -> Unit)
    | _ -> Unit
  in worker code

(* 
  Convertit et applique la closure conversion sur le code principal
  Paramètres:
  - code -> le code principal
  - funcs -> les headers des fonctions du programme
  Retourne: le code convertit avec la closure conversion appliquée dessus
*)
let convert_main (code: Knorm.knorm_t) (funcs: funheader list): t =
  let is_func_name (var: Id.t): bool =
    let label = gen_label var in
    List.exists (fun x -> x.label = label) funcs
  in
  let find_header (var: Id.t): funheader =
    let label = gen_label var in
    List.find (fun x -> x.label = label) funcs
  in
  let rec worker (e: Knorm.knorm_t): t =
    match e with
    | Int i -> Int(i)
    | Var id -> Var(id)
    | Add(a, b) -> Add(a, b)
    | Sub(a, b) -> Sub(a, b)
    | IfEq((a, b), t, els) -> IfEq((a, b), worker t, worker els)
    | IfLE((a, b), t, els) -> IfLE((a, b), worker t, worker els)
    | Let((id, t), App(name, args), next) -> 
      if is_func_name name then
        let h = find_header name in
        if List.length h.frees = 0 then
          Let(id, ApplyDirect(gen_label name, args), worker next)
        else
          let new_id = Id.genid () in
          Let(new_id, MakeClosure(gen_label name, h.frees), Let(id, ApplyClosure(new_id, args), worker next))
      else
        Let(id, ApplyClosure(name, args), worker next)
    | Let((id, t), value, next) -> Let(id, worker value, worker next)
    | LetRec(fd, next) -> worker next
    | App(name, args) -> 
      if is_func_name name then
        let h = find_header name in
        if (List.length h.frees) = 0 then
          ApplyDirect(gen_label name, args)
        else 
          insert_let (MakeClosure(gen_label name, h.frees)) (fun x -> match x with | Var id -> ApplyClosure(id, args) | _ -> Unit)
        else if Typechecker.is_prefef_fun name then
          ApplyPredef(name, args)
        else
          ApplyClosure(name, args)
    | _ -> Unit
  in worker code

(* 
  Réalise la closure conversion d'un programme
  Paramètres:
  - ast -> l'ast d'un programme
  Retourne: l'ast du programme après closure conversion   
*)
let closure (ast: Knorm.knorm_t): t =
  let funcs = make_funcs_headers ast (find_funcs_names ast) in
  let headers = List.map (fun x -> fst x) funcs in
  let funcs = List.map (fun (x, y) -> (LetRec(x, convert_func_body x y headers))) funcs in
  Prog(funcs, convert_main ast headers)
