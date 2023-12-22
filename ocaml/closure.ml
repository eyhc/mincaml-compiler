open Printf

type t =
  | Unit
  | Int of int
  | Var of Id.t
  | Add of t * t
  | Sub of t * t
  (* Déclaration d'une variable: nom * valeur * in *)
  | Let of Id.t * t * t
  (* Définition d'une fonction: label * paramètres * code *)
  | Fun of fundef
  (* Appel à une fonction via apply_direct: label de la fonction * paramètres *)
  | Apply of Id.t * t list
  (* Programme: défitions de fonctions * code principal *)
  | Prog of t list * t
and fundef = {label: Id.t; args: Id.t list; body: t}

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
  | _ -> ""