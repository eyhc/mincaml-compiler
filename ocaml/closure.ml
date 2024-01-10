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
  | LetRec(fd, body) -> sprintf("label: %s\nfree variables: %s\nparameters: %s\ncode:\n%s\n") fd.label 
                          (if (List.length fd.frees) = 0 then "None" else infix_to_string Id.to_string fd.frees " ")
                          (if (List.length fd.args) = 0 then "None" else infix_to_string Id.to_string fd.args " ")
                          (to_string body)
  | MakeClosure(label, free_vars) -> sprintf "make_closure(%s, %s)" label (infix_to_string Id.to_string free_vars ", ")
  | ApplyDirect(label, args) -> sprintf "apply_direct(%s, %s)" label (infix_to_string Id.to_string args ", ")
  | ApplyClosure(name, args) -> sprintf "apply_closure(%s, %s)" name (infix_to_string Id.to_string args ", ")
  | ApplyPredef(name, args) -> sprintf "%s %s" name (infix_to_string Id.to_string args " ")
  | Prog(funs, main) -> sprintf "%s\n%s" (infix_to_string to_string funs "\n") (to_string main)
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
  | Let((id, t), value, next) -> [id]
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
    | App(name, a) -> printf("%s\n") (infix_to_string Id.to_string a " "); List.filter_map (fun x -> if is_free x then Some x else None) a
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
let rec make_funcs_headers (ast: Knorm.knorm_t): (funheader * Knorm.knorm_t) list =
  let funcs_names = find_funcs_names ast in
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
    | Let((id, t), value, next) -> make_funcs_headers next
    | LetRec(fd, next) -> make_func_header fd None @ make_funcs_headers next
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
  let rec worker (e: Knorm.knorm_t): t =
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
    | Add(a, b) -> Add(a, b)
    | Sub(a, b) -> Sub(a, b)
    | IfEq((a, b), t, els) -> IfEq((a, b), worker t, worker els)
    | IfLE((a, b), t, els) -> IfLE((a, b), worker t, worker els)
    | Let((id, t), value, next) -> Let(id, worker value, worker next)
    | LetRec(fd, next) -> worker next
    | App(name, a) -> let h = find_header name in
                         if List.length h.frees = 0 then
                          ApplyDirect(gen_label name, h.args)
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
  let rec worker (e: Knorm.knorm_t): t =
    match e with
    | Int i -> Int(i)
    | Var id -> Var(id)
    | Add(a, b) -> Add(a, b)
    | Sub(a, b) -> Sub(a, b)
    | IfEq((a, b), t, els) -> IfEq((a, b), worker t, worker els)
    | IfLE((a, b), t, els) -> IfLE((a, b), worker t, worker els)
    | Let((id, t), value, next) -> Let(id, worker value, worker next)
    | LetRec(fd, next) -> worker next
    | App(name, args) -> if is_func_name name then
                           ApplyDirect(gen_label name, args)
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
  let funcs = make_funcs_headers ast in
  let headers = List.map (fun x -> fst x) funcs in
  let funcs = List.map (fun (x, y) -> (LetRec(x, convert_func_body x y headers))) funcs in
  Prog(funcs, convert_main ast headers)

(* (* 
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
  let frees = find_free_vars fd.body args locals in
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

(* 
  Créér les en-têtes des fonctions d'un programme
  Paramètres:
  - ast -> l'ast d'un programme
  Retourne: une liste de (funheader * Knorm.knorm_t)
*)
let rec make_funcs_headers (ast: Knorm.knorm_t): (funheader * Knorm.knorm_t) list =
  (*** Code de make_funcs_headers  ***)
  match ast with
  | Let((id, t), value, next) -> make_funcs_headers next
  | LetRec(fd, next) -> make_func_header fd None @ make_funcs_headers next
  | _ -> []

(* 
  Applique la closure conversion sur le code d'une fonction
  Paramètres:
  - head -> l'en-tête de la fonction
  - headers -> les en-têtes des autres fonctions du programme
  - body -> son code
  Retourne: le code de la fonction convertit après la closure conversion   
*)
let convert_func_body (head: funheader) (headers: funheader list) (body: Knorm.knorm_t): t =
  (* let rec find_makeclosure_args (f: funheader) (frees: Id.t list): Id.t list =
    let args = List.filter_map (fun x -> if List.exists (fun y -> y = x) frees then Some x else None) (f.args @ f.locals) in
    if List.length args = List.length frees then
      args
    else
      match f.parent with
      | None -> args
      | Some parent -> args @ find_makeclosure_args !parent frees
  in *)
  let is_local (var: Id.t): bool =
    List.exists (fun x -> x = var) head.locals
  in
  let has_free_vars (label: Id.t) =
    let h = List.find (fun x -> x.label = label) headers in
    List.length h.frees
  in
  let rec convert (e: Knorm.knorm_t): t =
    match e with
    | Unit -> Unit
    | Int i -> Int(i)
    | Var(id) -> if is_local id then 
                  Var(id) 
                else 
                  let label = gen_label id in
                  List.iter (fun x -> printf("%s\n") x.label) headers;
                  (* let frees = (List.find (fun x -> x.label = label) headers).frees in *)
                  insert_let (MakeClosure(gen_label id, [])) (fun x -> x)
    | Neg a -> Neg(a) 
    | Add(a, b) -> Add(a, b)
    | Sub(a, b) -> Sub(a, b)
    | IfEq((a, b), t, e) -> IfEq((a, b), convert t, convert e)
    | IfLE((a, b), t, e) -> IfLE((a, b), convert t, convert e)
    | Let((id, t), value, next) -> Let(id, convert value, convert next)
    | LetRec(fd, next) -> convert next
    | App(name, args) -> ApplyDirect(gen_label name, args)
    | _ -> Unit
  in convert body

(* 
  Réalise la closure conversion d'un programme
  Paramètres:
  - ast -> l'ast d'un programme
  Retourne: l'ast du programme après closure conversion   
*)
let closure (ast: Knorm.knorm_t): t =
  let funcs = make_funcs_headers ast in
  let headers = List.map (fun x -> fst x) funcs in
  let funcs = List.map (fun (x, y) -> (LetRec(x, (convert_func_body x [] y)))) funcs in
  printf("%s") (infix_to_string to_string funcs "\n");
  Unit







 *)
















(*(* 
  Retourne l'ensemble des noms des fonctions d'un programme
  Paramètres:
  - ast : l'ast d'un programme
  Retourne: une liste de nom de fonctions
*)
let rec get_funcs_names (e: Knorm.knorm_t): Id.t list =
  match e with
  | Let((id, t), value, next) -> get_funcs_names next
  | LetRec(fd, next) -> (fst fd.name) :: (get_funcs_names fd.body @ get_funcs_names next)
  | _ -> []

let rec infix_to_string (to_s : 'a -> string) (l : 'a list) (op : string) : string = 
  match l with 
  | [] -> ""
  | [x] -> to_s x
  | hd :: tl -> (to_s hd) ^ op ^ (infix_to_string to_s tl op)

let insert_let e k =
  let name = Id.genid () in
  let e' = k (Var(name)) in
  Let(name, e, e')


(* 
  Réalise la closure conversion d'un programme
  Paramètres:
  - ast : l'ast d'un programme
  Retourne: l'ast du programme après closure conversion   
*)
let closure (ast: Knorm.knorm_t): t =
  (* La liste des noms des fonctions *)
  let funcs_names = get_funcs_names ast in
  let rec is_func (name: Id.t): bool =
    List.exists (fun x -> x = name) funcs_names
  in
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
    | App(name, args) -> if is_func name then 
                          ApplyDirect(gen_label name, args)
                         else
                          ApplyClosure(name, args)
    | _ -> Unit
  in
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
  in
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
      | Let((id, t), value, next) -> id :: extract_locals_vars next
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
        | LetRec(fd, next) -> extract next
        | App(name, args) -> List.filter_map (fun x -> if is_free x then Some x else None) args
        | _ -> []
      in
      (* remove_dups (extract body) *) extract body
    in
    let label = gen_label (fst fd.name) in
    let args = List.map (fun x -> fst x) fd.args in
    let locals = extract_locals_vars fd.body in
    let frees = extract_free_vars args locals fd.body in
    let rec convert_body (e: Knorm.knorm_t): t =
      match e with
      | Var id -> if is_func id then
                    insert_let (MakeClosure(gen_label id, args)) (fun x -> x)
                  else
                    Var(id)
      | Unit -> Unit
      | Int i -> Int(i)
      | Neg a -> Neg(a) 
      | Add(a, b) -> Add(a, b)
      | Sub(a, b) -> Sub(a, b)
      | IfEq((a, b), t, e) -> IfEq((a, b), convert_exp t, convert_exp e)
      | IfLE((a, b), t, e) -> IfLE((a, b), convert_exp t, convert_exp e)
      | Let((id, t), value, next) ->  Let(id, convert_exp value, convert_body next)
      | LetRec(fd, next) -> convert_body next
      | App(name, args) -> if is_func name then 
                            ApplyDirect(gen_label name, args)
                            else
                            ApplyClosure(name, args)
      | _ -> Unit
    in
    LetRec({label= label; args= args; frees=frees; body= convert_body fd.body})
  in
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
  | MakeClosure(label, free_vars) -> sprintf "make_closure(%s, %s)" label (infix_to_string Id.to_string free_vars ", ")
  | ApplyDirect(label, args) -> sprintf "apply_direct(%s, %s)" label (infix_to_string Id.to_string args ", ")
  | ApplyClosure(name, args) -> sprintf "apply_closure(%s, %s)" name (infix_to_string Id.to_string args ", ")
  | Prog(funs, main) -> sprintf "%s\n%s" (infix_to_string to_string funs "\n") (to_string main)
  | _ -> "" *)