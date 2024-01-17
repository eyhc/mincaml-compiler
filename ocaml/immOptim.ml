(*
ARMv5 & earlier
12 bits-immediate optimization

date : 16/01/2024
*)


(* Determine la liste des chiffres de l'entier sur 32 bits en base 4 *)
let bits (i:int) : int array =
  let rec bits_rec (i:int) (r:int) (a:int array) : unit =
    if r = 0 then ()
    else 
      let b = (i+4) mod 4 and i' = Int.shift_right i 2 in
      a.(16-r) <- b ;bits_rec i' (r-1) a
  in
  let a = Array.make 16 0 in 
  let _ = bits_rec i 16 a in a

(* Détermine le nombre de zeros successifs à partir de k dans a (modulo 16) *)
let nb_succ (a:int array) (k:int): int =
  let rec nb_succ_rec i =
    if i = (16+k-1) mod 16 then if a.(i) = 0 then 1 else 0
    else if a.(i) >= 1 then 0
    else 1 + nb_succ_rec ((i+1) mod 16)
  in
  nb_succ_rec k


(* Determine si un entier entre sur 8 bits moyennant une rotation *)
(* On cherche 12 chiffres à zéro consécutifs (modulo 16) *)
let can_be_imm (i:int) : bool =
  let arr = bits i in
    let l = List.init 16 (fun x -> x) in
      let l1 = List.map (nb_succ arr) l in
        List.exists (fun x -> x >= 12) l1;;


(************************************************************************)

(* Propage les valeurs immédiates *)
let propagation_imm (asml: Asml.asml) : Asml.asml =

  let rec optim_asml (asml:Asml.asml) : Asml.asml = 
    List.map optim_letdef asml

  and optim_letdef (a:Asml.letdef) : Asml.letdef =
    match a with
    | Main a -> Main (optim_asmt [] a)
    | LetFloat _ -> a
    | LetLabel (l,args,a) -> LetLabel (l, args, optim_asmt [] a)

  and optim_asmt (env:(Id.t*int) list) (a:Asml.asmt) : Asml.asmt =
    match a with
    | LET (s,e,a') -> 
      let e' = optim_expr env e in
        let env2 = (match e' with 
                    | VAL (Const i) -> if can_be_imm i then (s,i)::env else env 
                    | _ -> env) 
        in
          LET(s, e', optim_asmt env2 a')
    | EXP e -> EXP (optim_expr env e)

  and optim_expr (env:(Id.t*int) list) (a:Asml.expr) : Asml.expr =
    match a with
    | VAL vi -> VAL (optim_val_imm env vi)
    | ADD (s, vi) -> ADD (s, optim_val_imm env vi)
    | SUB (s, vi) -> SUB (s, optim_val_imm env vi)
    | NEW vi -> NEW (optim_val_imm env vi)
    | MEMGET (s, vi) -> MEMGET (s, optim_val_imm env vi)
    | MEMASSIGN (s, vi, s') -> MEMASSIGN (s, optim_val_imm env vi, s')
    | IFEQ ((s,vi), a1, a2) -> 
      IFEQ ((s, optim_val_imm env vi), optim_asmt env a1, optim_asmt env a2)
    | IFLE ((s,vi), a1, a2) -> 
      IFLE ((s, optim_val_imm env vi), optim_asmt env a1, optim_asmt env a2)
    | IFGE ((s,vi), a1, a2) -> 
      IFGE ((s, optim_val_imm env vi), optim_asmt env a1, optim_asmt env a2)
    | IFFEQUAL (c, a1, a2) -> IFFEQUAL (c, optim_asmt env a1, optim_asmt env a2)
    | IFFLE (c, a1, a2) -> IFFLE (c, optim_asmt env a1, optim_asmt env a2)
    | _ -> a

  and optim_val_imm (env:(Id.t*int) list) (vi:Asml.id_or_imm) : Asml.id_or_imm =
    match vi with
    | Var s -> 
      (try let i = snd (List.find (fun (x,_) -> x = s) env) in Const i
       with Not_found -> vi)
    | _ -> vi

  in optim_asml asml



(* Elimination de declarations inutiles *)
let elimination_imm (asml: Asml.asml) : Asml.asml =

  let rec elim_asml (asml:Asml.asml) : Asml.asml = 
    List.map elim_letdef asml

  and elim_letdef (a:Asml.letdef) : Asml.letdef =
    match a with
    | Main a -> Main (elim_asmt [] a)
    | LetFloat _ -> a
    | LetLabel (l,args,a) -> LetLabel (l, args, elim_asmt [] a)

  and elim_asmt (env:Elim.environment) (a:Asml.asmt) : Asml.asmt =
    match a with
    | LET (s,e,a1) ->
      (match e with
      | VAL (Const _) -> let env2 = (s, ref 0)::env in 
        let a2 = elim_asmt env2 a1 in
          if (Elim.env_get env2 s) <> 0 then LET(s, e, a2) else a2
      | _ -> LET(s, elim_expr env e, elim_asmt env a1))
    | EXP e -> EXP (elim_expr env e)

  and elim_expr (env:Elim.environment) (a:Asml.expr) : Asml.expr =
    match a with
    | VAL vi -> VAL (elim_val_imm env vi)
    | NEG s -> let _ = Elim.env_incr env s in a
    | ADD (s, vi) -> 
      let _ = Elim.env_incr env s in ADD (s, elim_val_imm env vi)
    | SUB (s, vi) -> let _ = Elim.env_incr env s in SUB (s, elim_val_imm env vi)
    | NEW vi -> NEW (elim_val_imm env vi)
    | MEMGET (s, vi) ->
      let _ = Elim.env_incr env s in MEMGET (s, elim_val_imm env vi)
    | MEMASSIGN (s, vi, s') -> 
      let _ = Elim.env_incr env s; Elim.env_incr env s' in
        MEMASSIGN(s, elim_val_imm env vi, s')
    
    | IFEQ ((s,vi), a1, a2) -> let _ = Elim.env_incr env s in
      IFEQ ((s, elim_val_imm env vi), elim_asmt env a1, elim_asmt env a2)
    | IFLE ((s,vi), a1, a2) -> let _ = Elim.env_incr env s in
      IFLE ((s, elim_val_imm env vi), elim_asmt env a1, elim_asmt env a2)
    | IFGE ((s,vi), a1, a2) -> let _ = Elim.env_incr env s in
      IFGE ((s, elim_val_imm env vi), elim_asmt env a1, elim_asmt env a2)

    | IFFEQUAL (c, a1, a2) -> IFFEQUAL (c, elim_asmt env a1, elim_asmt env a2)
    | IFFLE (c, a1, a2) -> IFFLE (c, elim_asmt env a1, elim_asmt env a2)

    | CALL (_, args) | CALLCLO (_, args) -> 
      let _ = List.iter (Elim.env_incr env) args in a
    | _ -> a

  and elim_val_imm (env:Elim.environment) (vi:Asml.id_or_imm) : Asml.id_or_imm =
    match vi with
    | Var s -> let _ = Elim.env_incr env s in vi
    | _ -> vi

  in elim_asml asml


(* Main *)
let optim (asml:Asml.asml) : Asml.asml =
  elimination_imm (propagation_imm asml)