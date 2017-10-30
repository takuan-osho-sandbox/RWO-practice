open Core_kernel

type 'a expr =
  | Base of 'a
  | Const of bool
  | And of 'a expr list
  | Or of 'a expr list
  | Not of 'a expr

type mail_field = To | From | CC | Date | Subject
type mail_predicate = {
  field: mail_field;
  contains: string
}

let test field contains = Base {
  field;
  contains;
}

let rec eval expr base_eval =
  (* a shortcut, so we don't need to repeatedly pass [base_eval] explicitly to [eval] *)
  let eval' expr = eval expr base_eval in
  match expr with
  | Base base -> base_eval base
  | Const bool -> bool
  | And exprs -> List.for_all exprs ~f:eval'
  | Or exprs -> List.exists exprs ~f:eval'
  | Not expr -> not (eval' expr)