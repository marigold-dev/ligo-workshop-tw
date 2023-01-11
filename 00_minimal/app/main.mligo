#include "../src/lib.mligo"

type storage = string
type action = unit

(** (action, storage) *)
type input = action * storage

(** (operation list, storage) *)
type output = operation list * storage

let main (_input: input) : output =
  let new_store = some_func () in
  ([], new_store)
