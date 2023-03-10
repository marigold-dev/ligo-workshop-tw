#import "./multisig/src/internal/parameter.mligo" "Multisig"

type storage =
  { admin : address (* multisig address *)
  ; borrowers : (address, tez) big_map
  ; interest_rate : nat }

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
  let () = assert_with_error (Tezos.get_amount () >= 5tez) "The minimal amount should be 5tez" in
  [], store

type proposal_content = Multisig.Types.proposal_content


let borrow
  (loan_amount, borrower : tez * address)
  (store : storage)
  : output =
  let () = assert_with_error (not (Big_map.mem borrower store.borrowers)) "Can't borrow twice" in
  let create_proposal_entrypoint_opt =
    (Tezos.get_entrypoint_opt
      "%create_proposal"
      store.admin : (((unit proposal_content) list) contract) option)
  in
  let create_proposal_entrypoint = Option.unopt create_proposal_entrypoint_opt in
  let tx = [Transfer { target = borrower; parameter = (); amount = loan_amount }] in
  let op = Tezos.transaction tx 0tez create_proposal_entrypoint in
  let new_store = { store with borrowers = (Big_map.add borrower loan_amount store.borrowers) }in
  [op], new_store

let repay
  (store : storage)
  : output =
  let loan_opt = Big_map.find_opt (Tezos.get_sender ()) store.borrowers in
  let loan = Option.unopt loan_opt in
  let () = assert_with_error (Tezos.get_amount () > loan + store.interest_rate * 1tz) "not enough to repay" in
  let admin_contract = Tezos.get_contract_with_error store.admin "doesn't exist" in
  let op = Tezos.transaction (Tezos.get_amount ()) 0tez admin_contract in
  [op], store

let interest_rate
  ( interest_rate : nat)
  (store : storage)
  : output =
  let () = assert_with_error (store.admin = Tezos.get_sender ()) "not admin" in
  [], { store with interest_rate = interest_rate }
