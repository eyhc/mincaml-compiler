(*
asml.ml

date : 09-02-2023
*)

open Printf

(************************
    Types definitions
 ************************)


(* CF Syntax definition on ../doc/asml.html (at the bottom of the page) *)

type id_or_imm = Var of Id.t | Const of int

type expr =
| NOP
| VAL of id_or_imm
| LABEL of Id.l
| NEG of Id.t
| ADD of Id.t * id_or_imm
| SUB of Id.t * id_or_imm
| FNEG of Id.t
| FADD of Id.t * Id.t
| FSUB of Id.t * Id.t
| FMUL of Id.t * Id.t
| FDIV of Id.t * Id.t
| NEW of id_or_imm
| MEMGET of Id.t * id_or_imm
| MEMASSIGN of Id.t * id_or_imm * Id.t
| IFEQ of (Id.t*id_or_imm) * asmt * asmt
| IFLE of (Id.t * id_or_imm) * asmt * asmt
| IFGE of (Id.t * id_or_imm) * asmt * asmt
| IFFEQUAL of (Id.t*Id.t) * asmt * asmt
| IFFLE of (Id.t*Id.t) * asmt * asmt
| CALL of Id.l * Id.t list
| CALLCLO of Id.t * Id.t list
and asmt =
| LET of Id.t * expr * asmt (* let t = exp in asmt *)
| EXP of expr
and letdef =
| Main of asmt
| LetFloat of float
| LetLabel of Id.l * Id.t list * asmt
and asml = letdef list


(************************
  Generation functions
************************)
let call_predef (f:Id.l) (vars:Id.t list) : expr = 
  match f with
  | "print_int" -> CALL ("_min_caml_print_int", vars)
  | "print_float" -> CALL ("_min_caml_print_float", vars)
  | "print_newline" -> CALL ("_min_caml_print_newline", vars)
  | "sin" -> CALL ("_min_caml_sin", vars)
  | "cos" -> CALL ("_min_caml_cos", vars)
  | "sqrt" -> CALL ("_min_caml_sqrt", vars)
  | "abs" -> CALL ("_min_caml_abs", vars)
  | "abs_float" -> CALL ("_min_caml_abs", vars)
  | "int_of_float" -> CALL ("_min_caml_abs", vars)
  | "float_of_int" -> CALL ("_min_caml_float_of_int", vars)
  | "truncate" -> CALL ("_min_caml_truncate", vars)
  | _ -> failwith (sprintf "asml generation : %s not a predef function" f)

let funsdef: Closure.fundef list ref = ref [];;

let get_fdef (name: Id.t): Closure.fundef =
  List.find (fun (x: Closure.fundef) -> fst x.label = name) !funsdef

let is_freefun (name: Id.t): bool =
  if List.exists (fun (x: Closure.fundef) -> (fst x.label) = name) !funsdef then
    let f = List.find (fun (x: Closure.fundef) -> (fst x.label) = name) !funsdef in
    List.length f.frees > 0
  else
    false

let rec frees_mem_assign (p: Id.t) (n: int) (frees: Id.t list) (next: asmt): asmt =
  match frees with
  | [x] -> LET(Id.genid (), MEMASSIGN(p, Const(n), x), next)
  | x :: tail -> LET(Id.genid (), MEMASSIGN(p, Const(n), x), frees_mem_assign p (n+4) tail next)
  | _ -> assert false

let make_closure (id: Id.t) (f: Id.t) (frees: Id.t list) (next: asmt): asmt =
  let size = (List.length frees + 1) * 4 in
  let addr_id = Id.genid () in
  let mem_frees = frees_mem_assign id 0 (addr_id :: frees) next in
  let f_addr = LET(addr_id, LABEL(f), mem_frees) in
  let pointer = LET(id, NEW(Const(size)), f_addr) in
  pointer

let rec generation_let_with_apply (id: Id.t) (f: Id.t) (args: Id.t list) (next: Closure.t): asmt =
  (* L'ensemble des arguments qui sont des fonctions avec au moins une variable libre *)
  let freefuns_args = List.filter (fun x -> is_freefun x) args in
  if List.length freefuns_args > 0 then
    (* La liste des arguments à remplacer dans le call *)
    let args_ids = (List.map (fun x -> (x, Id.genid ())) freefuns_args) in
    let get_id (x: Id.t) : Id.t =
      snd (List.find (fun (y, z) -> y = x) args_ids)
    in
    let rec funs_as_params (names: Id.t list) (next: asmt): asmt =
      match names with
      | [x] -> let f = get_fdef x in make_closure (get_id x) (fst f.label) (List.map fst f.frees) next
      | x :: tail -> let f = get_fdef x in make_closure (get_id x) (fst f.label) (List.map fst f.frees) (funs_as_params tail next)
      | _ -> assert false
    in
    (* Le call avec les arguments mis à jour *)
    let call = CALL(f, List.map (fun x -> if List.exists (fun y -> (fst y) = x) args_ids then snd (List.find (fun y -> (fst y) = x) args_ids) else x) args) in
    let call = LET(id, call, generation_asmt next) in
    funs_as_params freefuns_args call
  else
    LET(id, 
    (if Typechecker.is_prefef_fun f then
      call_predef f args
    else
      CALL(f, args))
    , generation_asmt next)

and generation_expr (a:Closure.t) : expr =
  match a with
  | Unit -> NOP
  | Int i -> VAL (Const i)
  | Var id -> VAL (Var id)

  | Neg x -> NEG x
  | Add (x, y) -> ADD (x, Var y)
  | Sub (x, y) -> SUB (x, Var y)

  | IfEq (x, y, at1, at2) -> 
    IFEQ((x, Var y), generation_asmt at1, generation_asmt at2)
  | IfLE (x, y, at1, at2) ->
    IFLE((x, Var y), generation_asmt at1, generation_asmt at2)
  | ApplyDir(f, vars) -> if Typechecker.is_prefef_fun f then call_predef f vars else CALL(f, vars)
  | ApplyCls(l, vars) -> CALLCLO(l, vars)
  | _ -> assert false

and generation_asmt (a:Closure.t) : asmt = 
 match a with
 | MakeCls((x, t), l, args, e1) -> 
    let f = get_fdef l in
    make_closure x (fst f.label) (List.map fst f.frees) (generation_asmt e1)
 | Let((x, t), ApplyDir(f, args), e1) -> generation_let_with_apply x f args e1
 | Let ((x, t), e1, e2) -> LET(x, generation_expr e1, generation_asmt e2)
 | _ -> EXP (generation_expr a)

let rec generation_letdef (a:Closure.fundef) : letdef =
  let rec add_load_frees (n: int) (frees: Id.t list) (code: asmt) : asmt =
    match frees with
    | [] -> code
    | [x] -> LET(x, MEMGET("%self", Const(n)), code)
    | x :: tail -> LET(x, MEMGET("%self", Const(n)), add_load_frees (n+4) tail code)
  in
  LetLabel (fst a.label, List.map fst a.args, add_load_frees 4 (List.map fst a.frees) (generation_asmt a.code))

let rec generation (ast:Closure.t) : asml = 
 match ast with
 | Prog (fcts, main) -> 
    funsdef := fcts;
    (List.map generation_letdef fcts) @ [Main (generation_asmt main)]
 | _ -> failwith "Not correct closure form"


(************************
   To string functions
************************)

let to_string_id_imm (x:id_or_imm) : string =
 match x with
 | Var x -> Id.to_string x
 | Const i -> string_of_int i

let rec to_string_exp ?(p: string = "") (e:expr) = 
  let string_if(v1: string) (v2: string) (a1: asmt) (a2: asmt) (op: string): string =
    let sa1 = to_string_asmt (p^"  ") a1 and sa2 = to_string_asmt (p^"  ") a2 in
    sprintf "if %s %s %s then\n%s\n%selse\n%s\n" v1 op v2 sa1 p sa2
  in
  match e with
  | NOP -> sprintf "nop"
  | VAL v -> sprintf "%s" (to_string_id_imm v)
  | LABEL l -> sprintf "%s" (Id.to_string l)
  | NEG v -> sprintf "neg %s" (Id.to_string v)
  | ADD (v1,v2) -> sprintf "%sadd %s %s" p (Id.to_string v1) (to_string_id_imm v2)
  | SUB (v1,v2) -> sprintf "%ssub %s %s" p (Id.to_string v1) (to_string_id_imm v2)
  | FNEG v -> sprintf "(fneg %s)" (Id.to_string v)
  | FADD (v1,v2) -> sprintf "(fadd %s %s)" (Id.to_string v1) (Id.to_string v2)
  | FSUB (v1,v2) -> sprintf "(fsub %s %s)" (Id.to_string v1) (Id.to_string v2)
  | FMUL (v1,v2) -> sprintf "(fmul %s %s)" (Id.to_string v1) (Id.to_string v2)
  | FDIV (v1,v2) -> sprintf "(fdiv %s %s)" (Id.to_string v1) (Id.to_string v2)
  | NEW v -> sprintf "new %s" (to_string_id_imm v)
  | MEMGET (v1,v2) -> sprintf "mem(%s + %s)" (Id.to_string v1) (to_string_id_imm v2)
  | MEMASSIGN (v1,v2,v3) -> sprintf "mem(%s + %s) <- %s" v1 (to_string_id_imm v2) v3
  | IFEQ ((v, vd), a1, a2) -> string_if v (to_string_id_imm vd) a1 a2 "="
  | IFLE ((v, vd), a1, a2) -> string_if v (to_string_id_imm vd) a1 a2 "<="
  | IFGE ((v, vd), a1, a2) -> string_if v (to_string_id_imm vd) a1 a2 ">="
  | IFFEQUAL ((v, v2), a1, a2) -> string_if v v2 a1 a2 "=."
  | IFFLE ((v, v2), a1, a2) -> string_if v v2 a1 a2 "<=."
  | CALL (label,args) -> sprintf "call %s %s"
    (Id.to_string label) (Syntax.infix_to_string Id.to_string args " ")
  | CALLCLO (label,args) -> sprintf "call_closure %s %s" label (Syntax.infix_to_string Id.to_string args " ")

and to_string_asmt (p: string) (a:asmt) : string =
  match a with
  | LET (v, e, a) -> 
    sprintf "%slet %s = %s in\n%s" p (Id.to_string v) (to_string_exp e) (to_string_asmt p a)
  | EXP e -> p ^ (to_string_exp ~p:p e)

let rec to_string_letdef (l:letdef) : string =
  match l with
  | Main asmt -> sprintf "let _ = \n%s" (to_string_asmt "  " asmt)
  | LetFloat f -> Float.to_string f
  | LetLabel (l, args, asmt) -> sprintf "let %s %s =\n%s\n" l (Syntax.infix_to_string Id.to_string args " ") (to_string_asmt "  " asmt)

let to_string (a:asml) : string =
  List.fold_left (fun acc s -> acc ^ "\n" ^ (to_string_letdef s)) "" a


(* ======== POUR JORANNE ======== *)
let string_id (x:id_or_imm) : string =
  match x with
  | Var x -> sprintf "Var (\"%s\")" (Id.to_string x)
  | Const i -> sprintf "Const (%d)" i

let rec string_exp (e:expr) = 
  match e with
  | NOP -> "NOP"
  | VAL v -> sprintf "VAL (%s)" (string_id v)
  | LABEL l -> sprintf "LABEL (\"%s\")" (Id.to_string l)
  | NEG v -> sprintf "NEG (\"%s\")" (Id.to_string v)
  | ADD (v1,v2) -> sprintf "ADD (\"%s\", %s)" (Id.to_string v1) (string_id v2)
  | SUB (v1,v2) -> sprintf "SUB (\"%s\", %s)" (Id.to_string v1) (string_id v2)
  | FNEG v -> sprintf "FNEG (\"%s\")" (Id.to_string v)
  | FADD (v1,v2) -> sprintf "FADD (\"%s\", %s)" (Id.to_string v1) (Id.to_string v2)
  | FSUB (v1,v2) -> sprintf "FSUB (\"%s\", %s)" (Id.to_string v1) (Id.to_string v2)
  | FMUL (v1,v2) -> sprintf "FMUL (\"%s\", %s)" (Id.to_string v1) (Id.to_string v2)
  | FDIV (v1,v2) -> sprintf "FDIV (\"%s\", %s)" (Id.to_string v1) (Id.to_string v2)
  | NEW v -> sprintf "NEW (%s)" (string_id v)
  | MEMGET (v1,v2) -> sprintf "MEMGET (\"%s\", %s)" (Id.to_string v1) (string_id v2)
  | MEMASSIGN (v1,v2,v3) -> sprintf "(MEMASSIGN (\"%s\", %s, \"%s\")" v1 (string_id v2) v3
  | IFEQ ((v, vd), a1, a2) ->
    let sa1 = string_asmt a1 and sa2 = string_asmt a2 in
      sprintf "IFEQ ((\"%s\", %s), %s, %s)" (Id.to_string v) (string_id vd) sa1 sa2
  | IFLE ((v, vd), a1, a2) ->
    let sa1 = string_asmt a1 and sa2 = string_asmt a2 in
      sprintf "IFLE ((\"%s\", %s), %s, %s)" (Id.to_string v) (string_id vd) sa1 sa2
  | IFGE ((v, vd), a1, a2) ->
    let sa1 = string_asmt a1 and sa2 = string_asmt a2 in
      sprintf "IFGE ((\"%s\", %s), %s, %s)" (Id.to_string v) (string_id vd) sa1 sa2
  | IFFEQUAL ((v, v2), a1, a2) ->
    let sa1 = string_asmt a1 and sa2 = string_asmt a2 in
      sprintf "IFFEQUAL ((\"%s\", \"%s\"), %s, %s)" (Id.to_string v) (Id.to_string v2) sa1 sa2
  | IFFLE ((v, v2), a1, a2) ->
    let sa1 = string_asmt a1 and sa2 = string_asmt a2 in
      sprintf "IFFLE ((\"%s\", \"%s\"), %s, %s)" (Id.to_string v) (Id.to_string v2) sa1 sa2
  | CALL (label,args) -> sprintf "CALL (\"%s\", [%s])"
    (Id.to_string label) (Syntax.infix_to_string (fun x -> sprintf "\"%s\"" (Id.to_string x)) args ";")
  | CALLCLO (label,args) -> "CALLCLO (\"todo\", [])"

and string_asmt (a:asmt) : string =
  match a with
  | LET (v, e, a) -> 
    sprintf "LET (\"%s\", %s, %s)" (Id.to_string v) (string_exp e) (string_asmt a)
  | EXP e -> sprintf "EXP (%s)" (string_exp e)

let rec string_letdef (l:letdef) : string =
  match l with
  | Main asmt -> sprintf "Main (%s)" (string_asmt asmt)
  | LetFloat f -> failwith "todo"
  | LetLabel (l, args, asmt) -> failwith "todo"

let string_struct (a:asml) =
  sprintf "[%s]" (Syntax.infix_to_string string_letdef a ";")