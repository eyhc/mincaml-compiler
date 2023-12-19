(* une equation est un couple de types *)
type equation = Type.t * Type.t


(* l'environnement associe à un identifiant un type *)
type environment = (Id.t * Type.t) list


(* Environnement par défaut *)
let predef:environment = [
  ("print_int", Type.Fun ([Type.Int], Type.Unit)); 
  ("print_float", Type.Fun ([Type.Float], Type.Unit))
]


(* generation des equations d'analyse de types *)
let rec genEquations (expr:Syntax.t) (env:environment) (wanted:Type.t) : equation list =
  match expr with
  | Unit -> [ (Unit, wanted) ]
  | Bool b -> [ (Bool, wanted) ]
  | Int i -> [ (Int, wanted) ]
  | Float f -> [ (Float, wanted) ]
  | Not e -> let eq = genEquations e env Bool in
    (Bool, wanted) :: eq
  | Neg e -> let eq = genEquations e env Int in
    (Int, wanted) :: eq
  | Add (e1, e2) -> 
    let eq1 = genEquations e1 env Int and eq2 = genEquations e2 env Int in
      (Type.Int, wanted) :: eq1 @ eq2
  | Sub (e1, e2) -> 
    let eq1 = genEquations e1 env Int and eq2 = genEquations e2 env Int in
      (Type.Int, wanted) :: eq1 @ eq2
  | FNeg e -> 
    let eq = genEquations e env Float in
      (Type.Float, wanted) :: eq
  | FAdd (e1, e2) ->
    let eq1 = genEquations e1 env Float and eq2 = genEquations e2 env Float in
      (Type.Float, wanted) :: eq1 @ eq2
  | FSub (e1, e2) ->
    let eq1 = genEquations e1 env Float and eq2 = genEquations e2 env Float in
      (Type.Float, wanted) :: eq1 @ eq2
  | FMul (e1, e2) ->
    let eq1 = genEquations e1 env Float and eq2 = genEquations e2 env Float in
      (Type.Float, wanted) :: eq1 @ eq2
  | FDiv (e1, e2) ->
    let eq1 = genEquations e1 env Float and eq2 = genEquations e2 env Float in
      (Type.Float, wanted) :: eq1 @ eq2
  | Eq (e1, e2) -> failwith "todo"
  | LE (e1, e2) -> failwith "todo"
  | If (e1, e2, e3) -> 
    let eq1 = genEquations e1 env Bool in
      let eq2 = genEquations e2 env wanted and eq3 = genEquations e3 env wanted in
        eq1 @ eq2 @ eq3
  | Let ((id,t), e1, e2) -> 
    let eq1 = genEquations e1 env t and eq2 = genEquations e2 ((id,t)::env) wanted in
      eq1 @ eq2
  | Var id -> 
    (try
      let _,t1 = List.find (fun (x,y) -> x = id) env in [(t1, wanted)]
    with Not_found -> failwith (Printf.sprintf "Var %s not found" id))
  | App (e1, le2) -> 
    let t1 = Type.gentyp () and t2 = Type.gentyp () in
      let eq1 = genEquations e1 env t1 in
        (match le2 with
        | [] -> failwith "impossible"
        | e2::l2 ->
          if (l2 <> []) then failwith "App : multi params : todo";
          let eq2 = genEquations e2 env t2
            in  (t1, Type.Fun ([t2] , wanted)) :: eq1 @ eq2)
  | LetRec (fd, e) -> failwith "todo"
  | LetTuple (l, e1, e2)-> failwith "todo"
  | Get(e1, e2) -> failwith "todo"
  | Put(e1, e2, e3) -> failwith "todo"
  | Tuple(l) -> failwith "todo"
  | Array(e1,e2) -> failwith "todo"


(* convertit une liste d'équations en chaine de caractères *)
let rec to_string (x:equation list) : string =
  match x with
  | [] -> ""
  | e::l -> let (t1,t2) = e in
    Printf.sprintf "%s = %s ; %s" (Type.to_string t1) (Type.to_string t2) (to_string l)



(* resoud un systeme d'equations *)
let rec resolution (el:equation list) : equation list =
  match el with
  | [] -> []
  | (t1,t2)::l ->
    match t1,t2 with
      | Unit, Unit -> resolution l
      | Bool, Bool -> resolution l
      | Int, Int -> resolution l
      | Float, Float -> resolution l
      | Fun (l1, t1), Fun (l2, t2) -> failwith "todo"
      | Tuple l1, Tuple l2 -> failwith "todo"
      | Array t1, Array t2 -> failwith "todo"
      | Var v1,_ -> failwith "todo"
      | _ -> failwith "not unifiable"


(* replace each occurence of t1 by t2 in the equation list *)
let rec replace (l:equation list) (t1:Type.t) (t2:Type.t) : equation list =
  match l with
  | [] -> []
  | (e1,e2)::l1 ->
    let f1 = if e1 = t1 then t2 else e1 in
      let f2 = if e2 = t1 then t2 else e2 in
        (f1,f2)::(replace l1 t1 t2)




(* fonction de vérification de type *)
let typeCheck (expr:Syntax.t) : unit = 
  let l = genEquations expr predef Unit in 
    print_string (to_string l)