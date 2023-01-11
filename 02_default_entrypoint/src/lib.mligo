type storage = string

type action =
  | Default of unit
  | Borrow of (tez * address) (* (loan_amount, borrower) *)
  | Repay
  | InterestRate of nat

(** (action, storage) *)
type input = action * storage

(** (operation list, storage) *)
type output = operation list * storage

let default (store : storage) : output =
  let () = assert_with_error (Tezos.get_amount () >= 5tez) "the minimal amount should be 5tez" in
  [], store

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
