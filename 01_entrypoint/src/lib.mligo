type storage = string

type action =
  | Borrow of (tez * address) (* (loan_amount, borrower) *)
  | Repay
  | InterestRate of nat

(** (action, storage) *)
type input = action * storage

(** (operation list, storage) *)
type output = operation list * storage

let borrow
  (_loan_amount, _borrower : tez * address)
  (store : storage)
  : output = [], store

let repay
  (store : storage)
  : output = [], store

let interest_rate
  (_interest_rate : nat)
  (store : storage)
  : output = [], store
