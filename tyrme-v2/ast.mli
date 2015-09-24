(** Expressions of Tyrme. *)

type var = string

type value =
  | ValVar of var
  | Int of int
  | Bool of bool
  | String of string
  | Unit

type binop = Add | Sub | Mult | Div | Leq | Eq | And | Cat | App

type expr =
  | Var of var
  | Const of value
  | Binop of binop * expr * expr
  | If of expr * expr * expr
  | Let of var * expr * expr
  | Letf of var * var * expr * expr
  | Print of expr * expr
  | Pair of expr * expr
  | Tableau of expr list
  | Fst of expr
  | Snd of expr
  | Proj of int * expr
  | Projvar of expr * expr (*permet de recuperer la n-ieme case d'un tableau*)
  | Del of expr (* permet de supprimer le dernier élément d'un tableau*)
