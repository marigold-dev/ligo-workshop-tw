#import "ligo-breathalyzer/lib/lib.mligo" "Breath"
#import "../app/main.mligo" "App"
#import "./util.mligo" "Util"

(** init_storage *)
let init_storage admin =
  { admin = admin
  ; borrowers = (Big_map.empty : (address, tez) big_map)
  ; interest_rate = 0n }

(** ---------------------------  *)
(** originate *)
type originated = Breath.Contract.originated
let originate (level: Breath.Logger.level) (main : App.input -> App.output) (init_storage : App.storage) (amount : tez) =
  Breath.Contract.originate
    level
    "app"
    main
    init_storage
    amount

(** ---------------------------  *)
(** entrypoint *)
(** interest_rate *)
let interest_rate (contract : (App.action, App.storage) originated) (rate : nat) () =
  Breath.Contract.transfert_with_entrypoint_to contract "interestRate" rate 0tez

(** ---------------------------  *)
(** test case *)
let case_set_interest_rate_test =
  Breath.Model.case
  "test interest rate entrypoint and event"
  "success to set new interest rate and emit event"
    (fun (level: Breath.Logger.level) ->
      let (_baker, (alice, _bob, _carol)) = Breath.Context.init_default () in

      let contract = originate level App.main (init_storage alice.address) 0tez in

      let action = Breath.Context.act_as alice (interest_rate contract 1n) in

      let events = (Util.get_last_events_from contract.originated_address "new_interest_rate" : nat list) in

      let storage = Breath.Contract.storage_of contract in

      Breath.Result.reduce [
        action;
        Breath.Assert.is_equal "interest_rate" storage.interest_rate 1n;
        Breath.Assert.is_equal "event" (List.head_opt events) (Some 1n)
      ])

let test_suite =
  Breath.Model.suite "Suite for App" [
    case_set_interest_rate_test
  ; case_set_interest_rate_test
  ]

