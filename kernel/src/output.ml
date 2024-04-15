open! Core
open! Import
include Output_intf

type t =
  | Ansi
  | Ascii
  | Html
  | LaTeX 
[@@deriving compare, sexp]

let implies_unrefined t =
  match t with
  | Ansi | Html | LaTeX -> false
  | Ascii -> true
;;
