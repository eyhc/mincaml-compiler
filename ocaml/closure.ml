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
  | Neg of Id.t
  | Add of Id.t * Id.t
  | Sub of Id.t * Id.t
  (* If égal: (opérande gauche * opérande droite) * then * else  *)
  | IfEq of (Id.t * Id.t) * t * t
  (* If inférieur ou égal: (opérande gauche * opérande droite) * then * else  *)
  | IfLE of (Id.t * Id.t) * t * t
  (* Déclaration d'une variable: nom * valeur * in *)
  | Let of Id.t * t * t
  (* Définition d'une fonction: label * paramètres * variables libre * code *)
  | LetRec of fundef
  (* Création d'une closure: label de la fonction * variables libres *)
  | MakeClosure of Id.t * Id.t list
  (* Appel à une fonction via apply_direct: label de la fonction * paramètres *)
  | ApplyDirect of Id.t * Id.t list
  (* Appel d'une fonction prédéfinie: nom de la fonction * paramètres *)
  | ApplyPredef of Id.t * Id.t list
  (* Programme: fonctions du programme * code principal *)
  | Prog of t list * t
(* Définition d'une fonction: label * paramètres * variables libres * code *)
and fundef = {label: Id.t; args: Id.t list; frees: Id.t list; body: t}


(* 
  Génère un label pour une fonction
  Paramètres:
  - x -> le nom de la fonction
  Retourne: un label
*)
let gen_label (x: Id.t): Id.t = "_"^x

(* 
  Convertit les expressions de type Knorm.knorm_t en type t
  !! Ne pas convertir les fonctions avec cette fonction, utilisez convert_func !!
  Paramètres:
  - exp -> une expression de type Knorm.knorm_t
  Retourne: une expression équivalente de type t
*)
let rec convert_exp (exp: Knorm.knorm_t): t = 
  match exp with
  | Unit -> Unit
  | Int i -> Int(i)
  | Var(id) -> Var(id)
  | Neg a -> Neg(a) 
  | Add(a, b) -> Add(a, b)
  | Sub(a, b) -> Sub(a, b)
  | IfEq((a, b), t, e) -> IfEq((a, b), convert_exp t, convert_exp e)
  | IfLE((a, b), t, e) -> IfLE((a, b), convert_exp t, convert_exp e)
  | Let((id, t), value, next) -> Let(id, convert_exp value, convert_exp next)
  | LetRec(fd, next) -> convert_exp next
  | App(name, args) -> ApplyDirect(gen_label name, args)
  | _ -> Unit

(* 
  Extrait l'ensemble des fonctions d'un programme
  Paramètres:
  - ast -> l'ast d'un programme
  Retourne: une liste de fonctions   
*)
let rec extract_funcs (ast: Knorm.knorm_t): Knorm.fundef list =
  match ast with
  | Let((id, t), value, next) -> extract_funcs next
  | LetRec(fd, next) -> fd :: (extract_funcs fd.body @ extract_funcs next)
  | _ -> []

(* 
  Convertit une fonction et applique la closure conversion dessus
  Paramètres:
  - fd -> la définition d'une fonction 
  Retourne: la fonction convertit après closure conversion
*)
let convert_func (fd: Knorm.fundef): t =
  (* Extrait les variables locales de la fonction *)
  let rec extract_locals_vars (body: Knorm.knorm_t): Id.t list =
    match body with
    | Let((id, t), value, next) -> id :: (extract_locals_vars value @ extract_locals_vars next)
    | LetRec(fd, next) -> extract_locals_vars fd.body @ extract_locals_vars next
    | _ -> []
  in
  (* Extrait les variables libres de la fonction *)
  let rec extract_free_vars (args: Id.t list) (locals: Id.t list) (body: Knorm.knorm_t): Id.t list =
    let is_arg (var: Id.t): bool =
      List.exists (fun x -> x = var) args
    in
    let is_local_var (var: Id.t): bool =
      List.exists (fun x -> x = var) locals
    in
    let is_free (var: Id.t): bool =
      Bool.not (is_arg var || is_local_var var)
    in
    let add_free_var (var: Id.t): Id.t list =
      if is_free var then [var] else []
    in
    let rec extract (exp: Knorm.knorm_t): Id.t list =
      match exp with
      | Var id -> add_free_var id
      | Add(a, b) | Sub(a, b) -> add_free_var a @ add_free_var b
      | IfEq((a, b), t, e) | IfLE((a, b), t, e) -> add_free_var a @ add_free_var b @ extract t @ extract e
      | Let((id, t), value, next) -> extract value @ extract next
      | App(name, args) -> List.filter_map (fun x -> if is_free x then Some x else None) args
      | _ -> []
    in
    remove_dups (extract body)
  in
  let label = gen_label (fst fd.name) in
  let args = List.map (fun x -> fst x) fd.args in
  let locals = extract_locals_vars fd.body in
  let frees = extract_free_vars args locals fd.body in
  LetRec({label= label; args= args; frees=frees; body= convert_exp fd.body})


(* 
  Extrait et convertit le code principal d'un programme
  Paramètres:
  - ast -> l'ast d'un programme
  Retourne: le code principal convertit du programme   
*)
let rec convert_main (ast: Knorm.knorm_t): t =
  match ast with
  | LetRec(fd, next) -> convert_exp next
  | _ -> convert_exp ast


(* 
  Réalise la closure conversion d'un programme
  Paramètres:
  - ast : l'ast d'un programme
  Retourne: l'ast du programme après closure conversion   
*)
let closure (ast: Knorm.knorm_t): t =
  let funcs = List.map (fun x -> convert_func x) (extract_funcs ast) in
  Prog(funcs, convert_main ast)

(************************
   To string functions
************************)

let rec infix_to_string (to_s : 'a -> string) (l : 'a list) (op : string) : string = 
  match l with 
  | [] -> ""
  | [x] -> to_s x
  | hd :: tl -> (to_s hd) ^ op ^ (infix_to_string to_s tl op)

let rec to_string (exp: t): string =
  match exp with
  | Int i -> string_of_int i
  | Var id -> Id.to_string id
  | Neg a -> sprintf "-%s" (Id.to_string a)
  | Add(a, b) -> sprintf "%s + %s" (Id.to_string a) (Id.to_string b)
  | Sub(a, b) -> sprintf "%s - %s" (Id.to_string a) (Id.to_string b)
  | IfEq((a, b), t, e) -> sprintf "if %s = %s then\n%s\nelse\n%s\n" (Id.to_string a) (Id.to_string b) (to_string t) (to_string e)
  | IfLE((a, b), t, e) -> sprintf "if %s <= %s then\n%s\nelse\n%s\n" (Id.to_string a) (Id.to_string b) (to_string t) (to_string e)
  | Let(id, value, next) -> sprintf "let %s = %s in\n%s" (Id.to_string id) (to_string value) (to_string next)
  | LetRec(fd) -> sprintf("label: %s\nfree variables: %s\nparameters: %s\ncode:\n%s\n") fd.label 
                          (if (List.length fd.frees) = 0 then "None" else infix_to_string Id.to_string fd.frees " ")
                          (if (List.length fd.args) = 0 then "None" else infix_to_string Id.to_string fd.args " ")
                          (to_string fd.body)
  | ApplyDirect(label, args) -> sprintf "apply_direct(%s, %s)" label (infix_to_string Id.to_string args ", ")
  | Prog(funs, main) -> sprintf "%s\n%s" (infix_to_string to_string funs "\n") (to_string main)
  | _ -> ""