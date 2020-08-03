open Core_kernel
module Util = Libexecution.Util
open Libexecution
module RTT = Libexecution.Types.RuntimeT
open Libcommon
module T = Telemetry
module TS = Telemetry.Span

let collect
    (action : Stored_event.trim_events_action)
    (limit : int)
    (canvas : Stored_event.trim_events_canvases) =
  (* Outer span - one per loop invocation *)
  Telemetry.with_root "garbage_collector" (fun span ->
      (* Get all canvas ids *)
      let canvas_ids_and_names : (Uuidm.t * string) list =
        match canvas with
        | All ->
            T.with_span span "get_canvases_for_gc" (fun span ->
                let canvas_ids : (Uuidm.t * string) list =
                  Db.fetch
                    ~name:"canvases"
                    "SELECT id, name FROM canvases"
                    ~params:[]
                  (* List.map over all the rows; then List.hd_exn each row, it's is a single field, canvases.id *)
                  |> List.map ~f:(function
                         | [id; name] ->
                             (id |> Uuidm.of_string |> Tc.Option.value_exn, name)
                         | _ ->
                             Exception.internal "Wrong shape")
                in
                TS.set_attr span "canvas_count" (`Int (List.length canvas_ids)) ;
                canvas_ids)
        | JustOne canvas_name ->
            let canvas_id = Canvas.id_for_name canvas_name in
            [(canvas_id, canvas_name)]
      in
      TS.set_attr span "canvas_count" (`Int (List.length canvas_ids_and_names)) ;

      ( canvas_ids_and_names
      |> List.map ~f:(fun (canvas_id, canvas_name) ->
             (* Map over canvases. Do one canvas at a time for better cache locality *)
             let stored_events_v2_row_count =
               Telemetry.with_span
                 span
                 "garbage_collector_stored_events"
                 (fun span ->
                   let row_count =
                     Thread.yield () ;
                     Stored_event.trim_events_for_canvas
                       ~span
                       ~action
                       canvas_id
                       canvas_name
                       limit
                   in
                   TS.set_attr
                     span
                     "stored_events_v2_row_count"
                     (`Int row_count) ;
                   row_count)
             in
             let function_arguments_row_count =
               Telemetry.with_span
                 span
                 "garbage_collector_function_arguments"
                 (fun span ->
                   let row_count =
                     Thread.yield () ;
                     Stored_function_arguments.trim_arguments_for_canvas
                       span
                       action
                       canvas_id
                       ~canvas_name
                       ~limit
                   in
                   TS.set_attr
                     span
                     "function_arguments_row_count"
                     (`Int row_count) ;
                   row_count)
             in
             let function_results_v2_row_count =
               Telemetry.with_span
                 span
                 "garbage_collector_function_results_v2"
                 (fun span ->
                   let row_count =
                     Thread.yield () ;
                     Stored_function_result.trim_results_for_canvas
                       span
                       action
                       canvas_id
                       ~canvas_name
                       ~limit
                   in
                   TS.set_attr
                     span
                     "function_results_v2_row_count"
                     (`Int row_count) ;
                   row_count)
             in
             TS.set_attrs
               span
               [ ("stored_events_v2_row_count", `Int stored_events_v2_row_count)
               ; ( "function_arguments_row_count"
                 , `Int function_arguments_row_count )
               ; ( "function_results_v2_row_count"
                 , `Int function_results_v2_row_count ) ] ;
             ( stored_events_v2_row_count
             , function_arguments_row_count
             , function_results_v2_row_count ))
      (* Sum totals *)
      |> fun counts ->
      let ( stored_events_v2_counts
          , function_arguments_counts
          , function_results_v2_counts ) =
        List.unzip3 counts
      in
      TS.set_attrs
        span
        [ ( "stored_events_v2_row_count"
          , `Int (Tc.List.sum stored_events_v2_counts) )
        ; ( "function_arguments_row_count"
          , `Int (Tc.List.sum function_arguments_counts) )
        ; ( "function_results_v2_row_count"
          , `Int (Tc.List.sum function_results_v2_counts) ) ] ) ;
      ())
