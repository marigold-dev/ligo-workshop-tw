(* workround for [get_last_events_from] while have %default entrypoint in contract *)
let get_last_events_from (type a) (addr : address) (rtag: string) : a list =
  let event_map : (address * a) list = [%external ("TEST_LAST_EVENTS", rtag)] in
  let f ((acc, (c_addr,event)) : a list * (address * a)) : a list =
    if addr = c_addr then event::acc
    else acc
  in
  List.fold f event_map ([]: a list)
