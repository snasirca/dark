open Core_kernel
open Libexecution
open Analysis_types
open Types
module RTT = Types.RuntimeT
open Libcommon

(* ------------------------- *)
(* External *)
(* ------------------------- *)

let store
    ~canvas_id ~trace_id (tlid, fnname, id) (arglist : RTT.dval list) result =
  Db.run
    ~name:"stored_function_result.store"
    "INSERT INTO function_results_v2
     (canvas_id, trace_id, tlid, fnname, id, hash, hash_version, timestamp, value)
     VALUES ($1, $2, $3, $4, $5, $6, $7, CURRENT_TIMESTAMP, $8)"
    ~params:
      [ Uuid canvas_id
      ; Uuid trace_id
      ; ID tlid
      ; String fnname
      ; ID id
      ; String (Dval.hash Dval.current_hash_version arglist)
      ; Int Dval.current_hash_version
      ; RoundtrippableDval result ]


let load ~canvas_id ~trace_id tlid : function_result list =
  (* Right now, we don't allow the user to see multiple results when a function
   * is called in a loop. But, there's a lot of data when functions are called
   * in a loop, so avoid massive responses. *)
  Db.fetch
    ~name:"sfr_load"
    "SELECT
       DISTINCT ON (fnname, id, hash, hash_version)
       fnname, id, hash, hash_version, value, timestamp
     FROM function_results_v2
     WHERE canvas_id = $1
       AND trace_id = $2
       AND tlid = $3
     ORDER BY fnname, id, hash, hash_version, timestamp DESC"
    ~params:[Db.Uuid canvas_id; Db.Uuid trace_id; Db.ID tlid]
  |> List.map ~f:(function
         | [fnname; id; hash; hash_version; dval; ts] ->
             ( fnname
             , id_of_string id
             , hash
               (* hash_version is nullable, nulls come back as empty string *)
             , (match hash_version with "" -> 0 | hv -> hv |> int_of_string)
             , Dval.of_internal_roundtrippable_v0 dval )
         | _ ->
             Exception.internal
               "Bad DB format for stored_functions_results.load")


(** trim_results_for_canvas is like trim_results but for a single canvas.
 *
 * All the comments and warnings there apply. Please read them. *)
type trim_results_action = Stored_event.trim_events_action

let trim_results_for_handler
    (span : Libcommon.Telemetry.Span.t)
    (action : trim_results_action)
    ~(limit : int)
    ~(canvas_name : string)
    ~(tlid : string)
    (canvas_id : Uuidm.t) : int =
  let action_str = Stored_event.action_to_string action in
  Telemetry.with_span
    span
    "trim_results_for_handler"
    ~attrs:
      [ ("limit", `Int limit)
      ; ("canvas_id", `String (canvas_id |> Uuidm.to_string))
      ; ("canvas_name", `String canvas_name)
      ; ("tlid", `String tlid)
      ; ("action", `String action_str) ]
    (fun span ->
      let limit =
        (* Since we're deleting traces not in the main table, if the main table
         * has a lot of traces we might be in trouble, so cut it down a bit. *)
        let count =
          Db.fetch_count
            ~name:"count stored_events_v2"
            "SELECT COUNT(*) FROM stored_events_v2 WHERE canvas_id = $1 and tlid = $2"
            ~params:[Db.Uuid canvas_id; Db.String tlid]
        in
        if count > 1000000
        then limit / 100
        else if count > 100000
        then limit / 10
        else limit
      in
      let count =
        try
          (Stored_event.db_fn action)
            ~name:"gc_function_results"
            (Printf.sprintf
               "WITH event_ids AS (
                  SELECT trace_id
                  FROM stored_events_v2
                  WHERE canvas_id = $1
                    AND tlid = $2),
              to_delete AS (
                SELECT trace_id
                  FROM function_results_v2
                  WHERE canvas_id = $1
                    AND tlid = $2
                    ORDER BY timestamp ASC)
                  LIMIT $3)
              %s FROM function_results_v2
                WHERE canvas_id = $1
                  AND tlid = $2
                  AND trace_id IN (SELECT trace_id FROM event_ids)
                  AND trace_id IN (SELECT trace_id FROM to_delete);"
               action_str)
            ~params:[Db.Uuid canvas_id; Db.String tlid; Db.Int limit]
        with Exception.DarkException e ->
          Log.erroR
            "db error"
            ~params:
              [ ( "err"
                , e
                  |> Exception.exception_data_to_yojson
                  |> Yojson.Safe.to_string ) ] ;
          Exception.reraise (Exception.DarkException e)
      in
      Telemetry.Span.set_attr span "row_count" (`Int count) ;
      count)


let trim_results_for_canvas
    (span : Libcommon.Telemetry.Span.t)
    (action : trim_results_action)
    ~(limit : int)
    ~(canvas_name : string)
    (canvas_id : Uuidm.t) : int =
  Telemetry.with_span span "trim_results_for_canvas" (fun span ->
      let handlers =
        Telemetry.with_span
          span
          "get_function_handlers_for_canvas"
          ~attrs:[("canvas_name", `String canvas_name)]
          (fun span ->
            ( try
                Db.fetch
                  ~name:"get_function_handlers_for_gc"
                  "SELECT tlid
                   FROM toplevel_oplists
                   WHERE canvas_id = $1
                   AND tipe = 'user_function';"
                  ~params:[Db.Uuid canvas_id]
              with Exception.DarkException e ->
                Log.erroR
                  "db error"
                  ~params:
                    [ ( "err"
                      , e
                        |> Exception.exception_data_to_yojson
                        |> Yojson.Safe.to_string ) ] ;
                Exception.reraise (Exception.DarkException e) )
            (* List.hd_exn - we're only returning one field from this query *)
            |> List.map ~f:(fun tlid -> tlid |> List.hd_exn))
      in
      let row_count : int =
        handlers
        |> List.map ~f:(fun tlid ->
               trim_results_for_handler
                 span
                 action
                 ~tlid
                 ~canvas_name
                 ~limit
                 canvas_id)
        |> Tc.List.sum
      in
      Telemetry.Span.set_attrs
        span
        [ ("handler_count", `Int (handlers |> List.length))
        ; ("row_count", `Int row_count)
        ; ("canvas_name", `String canvas_name)
        ; ("canvas_id", `String (canvas_id |> Uuidm.to_string)) ] ;
      row_count)
