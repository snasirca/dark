open Core_kernel
open Libexecution
open Libbackend
open Libshared.FluidShortcuts
open Utils
open Libcommon
module SE = Libbackend.Stored_event

let span : Telemetry.Span.t =
  { name = "test"
  ; service_name = "test"
  ; span_id = 0
  ; trace_id = 0
  ; parent_id = 0
  ; start_time = Time.now ()
  ; attributes = String.Table.create () }


let ten_days_ago = Time.sub (Time.now ()) (Time.Span.create ~day:10 ())

let two_days_ago = Time.sub (Time.now ()) (Time.Span.of_day 2.0)

let check_int = AT.check AT.int

let setup_test (path : string) : Uuidm.t =
  clear_test_data () ;
  let c =
    ops2c_exn
      "test-host"
      [Types.SetHandler (tlid, pos, http_route_handler ~route:path ())]
  in
  Libbackend.Canvas.save_all !c ;
  !c.id


let store_data ~timestamp ~canvas_id handler (data : int) =
  let timestamp = Time.add timestamp (Time.Span.of_min (Int.to_float data)) in
  let trace_id = Util.create_uuid () in
  ignore
    (SE.store_event
       ~canvas_id
       ~trace_id
       ~timestamp
       handler
       (Dval.dstr_of_string_exn (Int.to_string data))) ;
  ()


let count_events_to_be_deleted canvas_id =
  Stored_event.trim_events_for_canvas
    ~span
    ~action:Count
    canvas_id
    "test-host"
    10000


let delete_events canvas_id =
  Stored_event.trim_events_for_canvas
    ~span
    ~action:Delete
    canvas_id
    "test-host"
    10000


let t_ten_most_recent () =
  let canvas_id = setup_test "/path" in

  let handler = ("HTTP", "/path", "GET") in
  let store_data = store_data ~canvas_id handler in

  store_data ~timestamp:two_days_ago 1 ;
  store_data ~timestamp:two_days_ago 2 ;
  store_data ~timestamp:two_days_ago 3 ;
  store_data ~timestamp:ten_days_ago 4 ;
  store_data ~timestamp:ten_days_ago 5 ;
  store_data ~timestamp:ten_days_ago 6 ;
  store_data ~timestamp:ten_days_ago 7 ;
  store_data ~timestamp:ten_days_ago 8 ;
  store_data ~timestamp:ten_days_ago 9 ;
  store_data ~timestamp:ten_days_ago 10 ;
  store_data ~timestamp:ten_days_ago 11 ;
  store_data ~timestamp:ten_days_ago 12 ;
  store_data ~timestamp:ten_days_ago 13 ;
  store_data ~timestamp:ten_days_ago 14 ;
  store_data ~timestamp:ten_days_ago 15 ;
  store_data ~timestamp:ten_days_ago 16 ;
  store_data ~timestamp:ten_days_ago 17 ;
  store_data ~timestamp:ten_days_ago 18 ;
  (* We expect that we keep 10 traces: 3 recent and 7 older *)
  check_int "expected is right" 8 (count_events_to_be_deleted canvas_id) ;
  check_int "deleted is right" 8 (delete_events canvas_id) ;
  let events = SE.load_events ~limit:20 ~canvas_id handler in
  AT.check AT.int "Only 10 remain" 10 (List.length events) ;
  ()


let t_keep_last_week () =
  let canvas_id = setup_test "/path" in

  let handler = ("HTTP", "/path", "GET") in
  let store_data = store_data ~canvas_id handler in
  store_data 1 ~timestamp:two_days_ago ;
  store_data 2 ~timestamp:two_days_ago ;
  store_data 3 ~timestamp:two_days_ago ;
  store_data 4 ~timestamp:two_days_ago ;
  store_data 5 ~timestamp:two_days_ago ;
  store_data 6 ~timestamp:two_days_ago ;
  store_data 7 ~timestamp:two_days_ago ;
  store_data 8 ~timestamp:two_days_ago ;
  store_data 9 ~timestamp:two_days_ago ;
  store_data 10 ~timestamp:two_days_ago ;
  store_data 11 ~timestamp:two_days_ago ;
  store_data 12 ~timestamp:two_days_ago ;
  store_data 13 ~timestamp:two_days_ago ;
  store_data 14 ~timestamp:two_days_ago ;
  store_data 15 ~timestamp:two_days_ago ;
  store_data 16 ~timestamp:two_days_ago ;
  store_data 17 ~timestamp:two_days_ago ;
  store_data 18 ~timestamp:two_days_ago ;
  store_data 19 ~timestamp:two_days_ago ;
  store_data 20 ~timestamp:two_days_ago ;
  store_data 21 ~timestamp:two_days_ago ;
  store_data 22 ~timestamp:ten_days_ago ;
  store_data 23 ~timestamp:ten_days_ago ;
  store_data 24 ~timestamp:ten_days_ago ;
  store_data 25 ~timestamp:ten_days_ago ;
  store_data 26 ~timestamp:ten_days_ago ;
  store_data 27 ~timestamp:ten_days_ago ;
  store_data 28 ~timestamp:ten_days_ago ;
  store_data 29 ~timestamp:ten_days_ago ;
  store_data 30 ~timestamp:ten_days_ago ;
  store_data 31 ~timestamp:ten_days_ago ;
  store_data 32 ~timestamp:ten_days_ago ;
  store_data 33 ~timestamp:ten_days_ago ;
  store_data 34 ~timestamp:ten_days_ago ;
  store_data 35 ~timestamp:ten_days_ago ;

  (* We expect that we keep 21 traces: all those younger than a week *)
  check_int "expected is right" 14 (count_events_to_be_deleted canvas_id) ;
  check_int "deleted is right" 14 (delete_events canvas_id) ;
  let events = SE.load_events ~limit:50 ~canvas_id handler in
  check_int "Only young remain" 21 (List.length events) ;
  ()


(* old garbage which matches no handler is completely gone. *)
let t_unmatched_garbage () =
  let canvas_id = setup_test "/path" in

  let store_data = store_data ~timestamp:ten_days_ago ~canvas_id in
  let good_handler = ("HTTP", "/path", "GET") in
  let f404_handler = ("HTTP", "/path", "POST") in
  store_data good_handler 1 ;
  store_data good_handler 2 ;
  store_data good_handler 3 ;
  store_data f404_handler 4 ;
  store_data f404_handler 5 ;
  store_data f404_handler 6 ;

  (* We expect that we keep 4 traces (good + latest 404) and delete 2
   * (non-latest 404s)) *)
  check_int "expected is right" 2 (count_events_to_be_deleted canvas_id) ;
  check_int "deleted is right" 2 (delete_events canvas_id) ;
  let f404 = SE.load_events ~canvas_id f404_handler in
  AT.check AT.int "1 404 remains" 1 (List.length f404) ;
  let good = SE.load_events ~canvas_id good_handler in
  AT.check AT.int "all good events remain" 3 (List.length good) ;
  ()


let t_wildcard_cleanup () =
  let path = "/:part1/other/:part2" in
  let canvas_id = setup_test path in

  let store_data segment1 segment2 =
    store_data
      ~timestamp:ten_days_ago
      ~canvas_id
      ("HTTP", "/" ^ segment1 ^ "/other/" ^ segment2, "GET")
      1
  in
  store_data "a1" "b1" ;
  store_data "a2" "b2" ;
  store_data "a3" "b3" ;
  store_data "a4" "b4" ;
  store_data "a5" "b5" ;
  store_data "a6" "b6" ;
  store_data "a7" "b7" ;
  store_data "a8" "b8" ;
  store_data "a9" "b9" ;
  store_data "a10" "b10" ;
  store_data "a11" "b11" ;
  store_data "a12" "b12" ;
  store_data "a13" "b13" ;
  store_data "a14" "b14" ;
  store_data "a15" "b15" ;
  store_data "a16" "b16" ;
  store_data "a17" "b17" ;
  store_data "a18" "b18" ;

  (* We expect that we keep 10 traces (good + latest 404) and delete 8 *)
  check_int "expected is right" 8 (count_events_to_be_deleted canvas_id) ;
  check_int "deleted is right" 8 (delete_events canvas_id) ;
  let good = SE.load_events ~canvas_id ("HTTP", path, "GET") in
  check_int "all good events remain" 10 (List.length good) ;
  ()


let suite =
  [ ("unmatched_garbage", `Quick, t_unmatched_garbage)
  ; ("ten most recent are saved", `Quick, t_ten_most_recent)
  ; ("keep all last week, regardless", `Quick, t_keep_last_week)
  ; ("wildcards are treated properly", `Quick, t_wildcard_cleanup) ]
