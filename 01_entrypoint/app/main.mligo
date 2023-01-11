#include "../src/lib.mligo"

let main (action, store: input) : output =
  match action with
  | Borrow b -> borrow b store
  | Repay -> repay store
  | InterestRate i -> interest_rate i store
