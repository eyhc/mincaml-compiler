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
 | IFLE of (Id.t*id_or_imm) * asmt * asmt
 | IFGE of (Id.t*id_or_imm) * asmt * asmt
 | IFFEQUAL of (Id.t*Id.t) * asmt * asmt
 | IFFLE of (Id.t*Id.t) * asmt * asmt
 | CALL of Id.l * Id.t list
 | CALLCLO of Id.t * Id.t list
 and asmt =
 | LET of Id.t * expr * asmt (* let t = exp in asmt *)
 | EXP of expr
 
 type letdef =
 | Main of asmt
 | LetFloat of float
 | LetLabel of Id.l * Id.t list * asmt
 
 type asml = letdef list

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
  | NEG v -> Printf.sprintf "(neg %s)" (Id.to_string v)
  | ADD (v1,v2) -> Printf.sprintf "(add %s %s)" (Id.to_string v1) (to_string_id_imm v2)
  | SUB (v1,v2) -> Printf.sprintf "(sub %s %s)" (Id.to_string v1) (to_string_id_imm v2)
  | FNEG v -> Printf.sprintf "(fneg %s)" (Id.to_string v)
  | FADD (v1,v2) -> Printf.sprintf "(fadd %s %s)" (Id.to_string v1) (Id.to_string v2)
  | FSUB (v1,v2) -> Printf.sprintf "(fsub %s %s)" (Id.to_string v1) (Id.to_string v2)
  | FMUL (v1,v2) -> Printf.sprintf "(fmul %s %s)" (Id.to_string v1) (Id.to_string v2)
  | FDIV (v1,v2) -> Printf.sprintf "(fdiv %s %s)" (Id.to_string v1) (Id.to_string v2)
  | NEW v -> Printf.sprintf "(new %s)" (to_string_id_imm v)
  | MEMGET (v1,v2) -> Printf.sprintf "mem(%s + %s)" (Id.to_string v1) (to_string_id_imm v2)
  | MEMASSIGN (v1,v2,v3) -> Printf.sprintf "(mem(%s + %s) <- %s)" v1 (to_string_id_imm v2) v3
  | IFEQ ((v, vd), a1, a2) ->
    let sa1 = to_string_asmt a1 and sa2 = to_string_asmt a2 in
      Printf.sprintf "(if %s = %s then\n%s\nelse\n%s)" (Id.to_string v) (to_string_id_imm vd) sa1 sa2
  | IFLE ((v, vd), a1, a2) ->
    let sa1 = to_string_asmt a1 and sa2 = to_string_asmt a2 in
      Printf.sprintf "(if %s <= %s then\n%s\nelse\n%s)" (Id.to_string v) (to_string_id_imm vd) sa1 sa2
  | IFGE ((v, vd), a1, a2) ->
    let sa1 = to_string_asmt a1 and sa2 = to_string_asmt a2 in
      Printf.sprintf "(if %s >= %s then\n%s\nelse\n%s)" (Id.to_string v) (to_string_id_imm vd) sa1 sa2
  | IFFEQUAL ((v, v2), a1, a2) ->
    let sa1 = to_string_asmt a1 and sa2 = to_string_asmt a2 in
      Printf.sprintf "(if %s =. %s then\n%s\nelse\n%s)" (Id.to_string v) (Id.to_string v2) sa1 sa2
  | IFFLE ((v, v2), a1, a2) ->
    let sa1 = to_string_asmt a1 and sa2 = to_string_asmt a2 in
      Printf.sprintf "(if %s <=. %s then\n%s\nelse\n%s)" (Id.to_string v) (Id.to_string v2) sa1 sa2
  | CALL (label,args) -> "call"
  | CALLCLO (label,args) -> "apply_closure"

  and to_string_asmt (a:asmt) : string =
  match a with
  | LET (v, e, a) -> 
    Printf.sprintf "(let %s = %s in\n%s)" (Id.to_string v) (to_string_exp e) (to_string_asmt a)
  | EXP e -> to_string_exp e

let rec to_string_letdef (l:letdef) : string =
  match l with
  | Main asmt -> to_string_asmt asmt
  | LetFloat f -> Float.to_string f
  | LetLabel (l, args, asmt) -> failwith "todo"

let to_string (a:asml) : string =
  List.fold_left (fun acc s -> acc ^ "\n" ^ (to_string_letdef s)) "" a


(************************
   Generation functions
 ************************)

let call_print_int x = CALL ("_min_caml_print_int", [x])


let rec generation_asmt (a:Syntax.t) : asmt = 
  EXP (NOP)

let generation (ast:Syntax.t) : asml = [Main (generation_asmt ast)]