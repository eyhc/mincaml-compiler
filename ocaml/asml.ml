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

let rec generation_expr (a:Closure.t) : expr =
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
  | ApplyDir(f, vars) -> if Typechecker.is_prefef_fun f then call_predef f vars else failwith "todo"

  (* | MakeClosure (f, ys) -> failwith "todo" *)
  | _ -> assert false


and generation_asmt (a:Closure.t) : asmt = 
 match a with
 | Let ((x, t), e1, e2) -> LET(x, generation_expr e1, generation_asmt e2)
 | _ -> EXP (generation_expr a)

let rec generation_letdef (a:Closure.fundef) : letdef =
  LetLabel (fst a.label, List.map fst a.args, generation_asmt a.code)

let rec generation (ast:Closure.t) : asml = 
 match ast with
 | Prog (fcts, main) -> (List.map generation_letdef fcts) @ [Main (generation_asmt main)]
 | _ -> failwith "Not correct closure form"


(************************
   To string functions
************************)

let to_string_id_imm (x:id_or_imm) : string =
 match x with
 | Var x -> Id.to_string x
 | Const i -> string_of_int i

let rec to_string_exp (e:expr) = 
  match e with
  | NOP -> "nop"
  | VAL v -> to_string_id_imm v
  | LABEL l -> Id.to_string l
  | NEG v -> sprintf "(neg %s)" (Id.to_string v)
  | ADD (v1,v2) -> sprintf "(add %s %s)" (Id.to_string v1) (to_string_id_imm v2)
  | SUB (v1,v2) -> sprintf "(sub %s %s)" (Id.to_string v1) (to_string_id_imm v2)
  | FNEG v -> sprintf "(fneg %s)" (Id.to_string v)
  | FADD (v1,v2) -> sprintf "(fadd %s %s)" (Id.to_string v1) (Id.to_string v2)
  | FSUB (v1,v2) -> sprintf "(fsub %s %s)" (Id.to_string v1) (Id.to_string v2)
  | FMUL (v1,v2) -> sprintf "(fmul %s %s)" (Id.to_string v1) (Id.to_string v2)
  | FDIV (v1,v2) -> sprintf "(fdiv %s %s)" (Id.to_string v1) (Id.to_string v2)
  | NEW v -> sprintf "(new %s)" (to_string_id_imm v)
  | MEMGET (v1,v2) -> sprintf "mem(%s + %s)" (Id.to_string v1) (to_string_id_imm v2)
  | MEMASSIGN (v1,v2,v3) -> sprintf "(mem(%s + %s) <- %s)" v1 (to_string_id_imm v2) v3
  | IFEQ ((v, vd), a1, a2) ->
    let sa1 = to_string_asmt a1 and sa2 = to_string_asmt a2 in
      sprintf "(if %s = %s then\n%s\nelse\n%s)" (Id.to_string v) (to_string_id_imm vd) sa1 sa2
  | IFLE ((v, vd), a1, a2) ->
    let sa1 = to_string_asmt a1 and sa2 = to_string_asmt a2 in
      sprintf "(if %s <= %s then\n%s\nelse\n%s)" (Id.to_string v) (to_string_id_imm vd) sa1 sa2
  | IFGE ((v, vd), a1, a2) ->
    let sa1 = to_string_asmt a1 and sa2 = to_string_asmt a2 in
      sprintf "(if %s >= %s then\n%s\nelse\n%s)" (Id.to_string v) (to_string_id_imm vd) sa1 sa2
  | IFFEQUAL ((v, v2), a1, a2) ->
    let sa1 = to_string_asmt a1 and sa2 = to_string_asmt a2 in
      sprintf "(if %s =. %s then\n%s\nelse\n%s)" (Id.to_string v) (Id.to_string v2) sa1 sa2
  | IFFLE ((v, v2), a1, a2) ->
    let sa1 = to_string_asmt a1 and sa2 = to_string_asmt a2 in
      sprintf "(if %s <=. %s then\n%s\nelse\n%s)" (Id.to_string v) (Id.to_string v2) sa1 sa2
  | CALL (label,args) -> sprintf "call %s %s" 
    (Id.to_string label) (Syntax.infix_to_string Id.to_string args " ")
  | CALLCLO (label,args) -> "apply_closure"

and to_string_asmt (a:asmt) : string =
  match a with
  | LET (v, e, a) -> 
    sprintf "(let %s = %s in\n%s)" (Id.to_string v) (to_string_exp e) (to_string_asmt a)
  | EXP e -> to_string_exp e

let rec to_string_letdef (l:letdef) : string =
  match l with
  | Main asmt -> sprintf "let _ = \n%s" (to_string_asmt asmt)
  | LetFloat f -> Float.to_string f
  | LetLabel (l, args, asmt) -> failwith "todo"

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