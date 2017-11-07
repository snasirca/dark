open Core

open Lib
open Runtime


module FnMap = String.Map

type fnmap = fn FnMap.t
let fns : fnmap =
  let add_fn (m : fnmap) (s : shortfn) : fnmap =
    let def = { name = s.n
              ; other_names = s.o
              ; return_type = s.r
              ; parameters = s.p
              ; description = s.d
              ; func = s.f
              ; preview = s.pr
              ; pure = s.pu
              } in
    List.fold_left ~f:(fun m1 n -> FnMap.add m1 ~key:n ~data:def) ~init:m (s.n::s.o)
  in
  List.fold_left ~f:add_fn ~init:FnMap.empty
    (* (List.concat [Stdlib.fns; Libtwitter.fns; Libdb.fns]) *)
    (List.concat [Stdlib.fns; Libdb.fns])

(* Give access to other modules *)
let get_fn (name : string) : fn option =
  FnMap.find fns name

let get_fn_exn (name : string) : fn =
  match FnMap.find fns name with
  | Some fn -> fn
  | None -> raise_error ("No function named '" ^ name ^ "' exists")
