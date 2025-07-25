(**************************************************************************)
(*  This file is part of COLORSTREAMS.                                    *)
(*                                                                        *)
(*  Copyright (C) 2025                                                    *)
(*    CEA (Commissariat à l'énergie atomique et aux énergies              *)
(*         alternatives)                                                  *)
(*                                                                        *)
(*  you can redistribute it and/or modify it under the terms of the GNU   *)
(*  Lesser General Public License as published by the Free Software       *)
(*  Foundation, version 2.1.                                              *)
(*                                                                        *)
(*  It is distributed in the hope that it will be useful,                 *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU Lesser General Public License for more details.                   *)
(*                                                                        *)
(*  See the GNU Lesser General Public License version 2.1                 *)
(*  for more details (enclosed in the file licenses/LGPLv2.1).            *)
(*                                                                        *)
(**************************************************************************)

type session =
    {
        name : string;
        o : in_channel;
        i : out_channel;
        err : in_channel;
        pid : int
    }

let create_session name cmd =
    let o, i, err =
        Message.Wrap.send (Message.Wrap.Debug("SUBPROCESS", lazy (Printf.sprintf "%s: %s" name cmd)));
        Unix.open_process_full cmd @@ Unix.environment ()
    in
    let pid = Unix.process_full_pid (o, i, err) in
    {name; o; i; err; pid}

let get_name s =
    s.name

let get_pid s =
    s.pid

let send s msg =
    if s.pid < 0
    then raise (Failure(Printf.sprintf "%s: killed" s.name))
    else
    (
        Message.Wrap.send (Message.Wrap.SmallDebug("SUBPROCESS", lazy (Printf.sprintf "%s <- %s" s.name msg)));
        Printf.fprintf s.i "%s\n" msg;
        flush s.i
    )

let get_line err chan s =
    let line = input_line chan in
    Message.Wrap.send (Message.Wrap.SmallDebug("SUBPROCESS", lazy (Printf.sprintf "%s%s -> %s" err s.name line)));
    line

let get_all_lines ?(cond = fun _ -> false) err chan s =
    let res = ref [] in
    try
        while true;
        do
            let line = get_line err chan s in
            res := line::!res;
            if cond line then raise End_of_file
        done;
        List.rev !res
    with End_of_file -> List.rev !res

let receive s = 
    get_line "" s.o s

let receive_err s =
    get_line "<err> " s.err s

let receive_until s cond =
    get_all_lines ~cond "" s.o s

let receive_errs_until s cond =
    get_all_lines ~cond "<err> " s.err s

let receive_all s =
    get_all_lines "" s.o s

let receive_all_errs s =
    get_all_lines "<err> " s.err s

let kill s =
    if s.pid > 0
    then
    (
        Unix.kill s.pid Sys.sigkill;
        {s with pid = (-1)}
    )
    else s

let close_session s =
    ignore @@ Unix.close_process_full (s.o, s.i, s.err);
    {s with pid = (-1)}

let log_errors ?(big = true) name s =
    let errs = receive_all_errs s in
    if not (errs = [])
    then
    (
        let err_msg = lazy (List.fold_left (fun res e -> res ^ e ^ "\n") (name ^ ":\n") errs) in
        if big
        then Message.Wrap.send (Message.Wrap.BigWarning(err_msg))
        else Message.Wrap.send (Message.Wrap.Warning(err_msg))
    )
    ;
    errs
