(*
knorm.ml

date : 08-01-2023
*)

open Printf

(* Le type knorm_t représente un ast k-normalisé
 * -> All nested expressions are replaced by new fresh variables *)
type knorm_t =
  | Unit
  | Var of Id.t
  | Int of int
  | Float of float
  | Bool of bool
  | Not of Id.t
  | Neg of Id.t
  | Add of Id.t * Id.t
  | Sub of Id.t * Id.t
  | FNeg of Id.t
  | FAdd of Id.t * Id.t
  | FSub of Id.t * Id.t
  | FMul of Id.t * Id.t
  | FDiv of Id.t * Id.t
  | If of bool_op * knorm_t * knorm_t
  | Let of (Id.t * Type.t) * knorm_t * knorm_t
  | LetRec of fundef * knorm_t
  | App of Id.t * Id.t list
  | LetTuple of (Id.t * Type.t) list * Id.t list * knorm_t
  | Tuple of Id.t list
  | Array of Id.t * Id.t
  | Get of Id.t * Id.t
  | Put of Id.t * Id.t * Id.t
and fundef = {name : Id.t * Type.t; args : (Id.t * Type.t) list; body : knorm_t }
and bool_op = Eq of Id.t * Id.t | LE of Id.t * Id.t

