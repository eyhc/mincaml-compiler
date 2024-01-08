open Printf

type t =
  | Unit
  | Int of int
  | Var of Id.t
  | Add of Id.t * Id.t
  | Sub of Id.t * Id.t
  (* Déclaration d'une variable: nom * valeur * in *)
  | Let of Id.t * t * t
  (* Définition d'une fonction: label * paramètres * code *)
  | Fun of fundef
  (* Appel à une fonction via apply_direct: label de la fonction * paramètres *)
  | Apply of Id.t * t list
  (* Appel d'une fonction prédéfinie: nom de la fonction * paramètres *)
  | Apply_Predef of Id.t * t list
  (* Programme: les fonctions du programme * code principal *)
  | Prog of t list * t
and fundef = {label: Id.t; args: Id.t list; body: t}

(* 
  Génère un label pour une fonction
  Paramètres:
  - x -> le nom de la fonction
  Retourne: un label
*)
let gen_label (x: Id.t): Id.t = "_"^x

(* 
  Insère une nouvelle variable
  Paramètres:
  - e -> la valeur de la nouvelle variable, une expression
  - k -> une fonction à appliquer pour le code suivant la nouvelle variable
  Retourne: un Let
*)
let insert_let e k =
  let name = Id.genid () in
  let e' = k (Var(name)) in
  Let(name, e, e')

(* 
  Flatten le code d'une fonction
  Paramètres:
  - e -> le code d'une fonction, une expression de type t
  Retourne: le code de la fonction après flatten, une expression de type t
*)
let rec flatten_fun (e: t): t =
  match e with
  | Int i -> e
  | Var id -> e
  | Add(a, b) -> insert_let (Add(a, b)) (fun x -> x)
  | Let(id, value, next) -> Let(id, value, flatten_fun next)

(* 
  Convertie une expression de type Knorm.knorm_t en type t
  Paramètres:
  - e -> une expression de type Knorm.knorm_t
  Retourne: une expression équivalente à e mais de type t
*)
let rec convert (e: Knorm.knorm_t): t =
  let convert_app (app: Knorm.knorm_t): t =
    let rec flatten_args args =
      match args with
      | [] -> []
      | [x] -> [Var(Id.to_string x)]
      | x :: tail -> Var(Id.to_string x) :: flatten_args args
    in
    match app with
    | App(n, a) -> let name = Id.to_string n in
                   let args = flatten_args a in
                   if Typechecker.is_prefef_fun name Typechecker.predef then
                    Apply_Predef(name, args)
                   else
                    Apply(gen_label name, args)
  in
  match e with
  | Var id -> Var(id)
  | Int i -> Int(i)
  | Add(a, b) -> Add(a, b)
  | Sub(a, b) -> Sub(a, b)
  | Let((id, t), value, next) -> Let(id, convert value, convert next)
  | LetRec(fd, next) -> convert next
  | App(name, args) -> convert_app e
  | _ -> Unit

(*
  Convertie une fonction de type Knorm.knorm_t en type t
  Paramètres:
  - f -> une fonction de type Knorm.knorm_t (fundef)
  Retourne: une fonction de type t   
*)
let convert_fun (f: Knorm.fundef): t =
  let rec convert_args args = 
    match args with
    | [] -> []
    | [(id, t)] -> [id]
    | (id, t) :: tail -> id :: convert_args tail
  in
  Fun({label= gen_label (fst f.name); args= (convert_args f.args); body= flatten_fun (convert f.body)})

(* 
  Extrait les fonctions du programme
  Paramètres:
  - ast -> l'ast du programme
  Retourne: la liste des fonctions du programmes
*)
let rec extract_funs (ast: Knorm.knorm_t): (Knorm.fundef list) =
  match ast with
  | Let((id, t), value, next) -> extract_funs next
  | LetRec(fd, next) -> fd :: extract_funs next
  | _ -> []

(* 
  Converti le code principal du programme
  Paramètres:
  - ast -> l'ast du programme
  Retourne: le code principal du programme converti  
*)
let rec extract_main (ast: Knorm.knorm_t): t =
  match ast with
  | LetRec(fd, next) -> convert next
  | _ -> convert ast

(* 
  Applique la closure conversion sur l'ast
  Paramètres:
  - ast : un ast de type Knorm.knorm_t
  Retourne: un ast de type t   
*)
let rec closure ast =
  let rec convert_funs funs =
    match funs with
    | [] -> []
    | [fd] -> [convert_fun fd]
    | fd :: tail -> convert_fun fd :: convert_funs tail
  in
  let funs = extract_funs ast in
  Prog(convert_funs funs, extract_main ast)


let rec to_string exp =
  let rec flatten args op =
    match args with
    | [] -> ""
    | [x] -> to_string x
    | head :: tail -> (to_string head) ^ op ^ (flatten tail op)
  in

let rec flatten_args args =
  match args with
  | [] -> ""
  | [x] -> x
  | head :: tail -> head ^ " " ^ (flatten_args tail)
in
  match exp with
  | Unit -> "()"
  | Int i -> string_of_int i
  | Var id -> id
  | Add(left, right) -> (Id.to_string left) ^ " + " ^ (Id.to_string right)
  | Sub(left, right) -> (Id.to_string left) ^ " - " ^ (Id.to_string right)
  | Let(id, value, next) -> sprintf("let %s = %s in\n%s") id (to_string value) (to_string next)
  | Fun(fd) -> sprintf("label: %s\nparams: %s\ncode:\n%s\n") (Id.to_string fd.label) (flatten_args fd.args) (to_string fd.body)
  | Apply(label, args) -> sprintf("apply_direct(%s, %s)") label (flatten args ", ")
  | Apply_Predef(name, args) -> sprintf("%s %s") name (flatten args " ")
  | Prog(funs, body) -> (flatten funs "\n") ^ "\n" ^ (to_string body) ^ "\n"
  | _ -> ""

(* 
  Converti une fonction
  Paramètres:
  - f -> une fonction de type KNorm.fd
  Retourne: une fonction de type fundef   
*)
(* let convert_fun (f: Knorm.fundef) =
  Fun({label= (gen_label (fst f.name)); args= []; body= (flatten_fun f.body (fun x -> x))}) *)

(*
  let genlabel (x: Id.t): Id.t = "_"^x

  let print_tuple (e1, e2) =
  printf("%s %s\n") e1 e2
 
(* Convertit les éléments de type Syntax.t en élément de type Closure.t *)
let conversion (exp: Syntax.t): t =
  (* Convertit les paramètres d'une fonction *)
  let rec convert_rec_args (args: (Id.t * Type.t) list): Id.t list =
    match args with
    | [] -> []
    | [(id, t)] -> [id]
    | (id, t) :: tail -> id :: (convert_rec_args tail)
  in

  (* Retourne la liste des labels des fonctions du programme *)
  let rec labelise_funs (e: Syntax.t): (Id.t * Id.t) list =
    match e with
    | Let((id, t), value, next) -> labelise_funs next
    | LetRec(fd, next) -> let name = fst fd.name in [(name, genlabel name)]@(labelise_funs next)
    | _ -> []
  in

  (* Retourne le label de la fonction *)
  let rec get_fun_label (labels: (Id.t * Id.t) list) (name: Id.t): Id.t =
    match labels with
    | [] -> ""
    | (n, l) :: tail -> if n = name then l else (get_fun_label tail name)
  in

  let labels = labelise_funs exp in

  let rec convert funs (e: Syntax.t): t =
    match e with
    | Unit -> Unit
    | Int i -> Int(i)
    | Var id -> Var(id)
    | Add(left, right) -> Add(convert funs left, convert funs right)
    | Sub(left, right) -> Sub(convert funs left, convert funs right)
    | Let((id,t), value, next) -> Let(id, convert funs value, convert funs next)
    | LetRec(fd, next) -> funs := !funs @ [Fun({label= get_fun_label labels (fst fd.name); args= convert_rec_args fd.args; body= convert funs fd.body})]; 
                          convert funs next
    | App(name, args) ->  let label = get_fun_label labels (Syntax.to_string name) in
                          (* Convertit les paramètres d'un App *)
                          let rec convert_app_args (args: Syntax.t list): t list =
                            match args with
                            | [] -> []
                            | [x] -> [convert funs x]
                            | head :: tail -> (convert funs head) :: (convert_app_args tail)
                          in Apply((if label = "" then (Syntax.to_string name) else label), convert_app_args args)
  in let convert_funs = ref [] in
     let main = convert convert_funs exp in
    Prog(!convert_funs, main)

    
let rec to_string exp =
  let rec flatten args op =
    match args with
    | [] -> ""
    | [x] -> to_string x
    | head :: tail -> (to_string head) ^ op ^ (flatten tail op)
  in

  let rec flatten_args args =
    match args with
    | [] -> ""
    | [x] -> x
    | head :: tail -> head ^ " " ^ (flatten_args tail)
  in

  match exp with
  | Unit -> "()"
  | Int i -> string_of_int i
  | Var id -> id
  | Add(left, right) -> (to_string left) ^ " + " ^ (to_string right)
  | Sub(left, right) -> (to_string left) ^ " - " ^ (to_string right)
  | Let(id, value, next) -> sprintf("let %s = %s in\n%s") id (to_string value) (to_string next)
  | Fun(fd) -> sprintf("label: %s\nparams: %s\ncode:\n%s\n") (Id.to_string fd.label) (flatten_args fd.args) (to_string fd.body)
  | Apply(label, args) -> sprintf("apply_direct(%s, %s)") label (flatten args ", ")
  | Prog(funs, body) -> (flatten funs "\n") ^ "\n" ^ (to_string body) ^ "\n"
  | _ -> "" *)