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

open Colorstreams
open Base
open Yojson.Safe

let json = ref ""

let p = ref []

module Wrap =
    struct
        type Message.Wrap.wrap += 
            | Policies
            | Error of string 
            | End 
            | Usage 

        let unwrapper next = function
            | Policies -> 
            (
                let msg = lazy (Printf.sprintf "Chosen policies: %s" @@ Utils.pp_list (fun (_, (module P : Policies.Policy.Sig)) -> P.name) !p) in
                0, false, "COLORSTREAMS", (Message.Value(msg)), msg
            )
            | Error(s) -> 0, true, "ERROR", (Message.Value(lazy s)), lazy s
            | End -> 0, false, "DONE", (Message.Value(lazy "")), lazy ""
            | Usage -> 
            (
                let msg = lazy (Options.pp_help ()) in
                0, false, "COLORSTREAMS", (Message.Value(msg)), msg
            )
            | w -> next w

        let _ = Message.Wrap.register_unwrapper unwrapper
    end

let result_mh = new Result.mh ()

let fini_again = ref false

let fini s =
    if s <> 0
    then
    (
        Message.clean ();
        if s = Sys.sigint
        then Message.Wrap.send (Wrap.Error("killed"))
        else if s = Sys.sigpipe
        then Message.Wrap.send (Wrap.Error("broken pipe (target may have crashed)"))
        else if s = 1
        then Message.Wrap.send (Wrap.Error("target crashed"))
        else if s = 200
        then Message.Wrap.send (Wrap.Error("general timeout"))
        else Message.Wrap.send (Wrap.Error("fatal error"))
        ;
        try
            Trace.kill ()
        with _ -> ()
    )
    ;
    if not !fini_again
    then
    (
        fini_again := true;
        List.iter (fun (p, (module P : Policies.Policy.Sig)) -> ignore @@ P.update (fun p -> P.A.fini s p) p) !p
    )
    else Message.Wrap.send (Wrap.Error("error in final callbacks"))
    ;
    Stats.log ();
    Message.Wrap.send Wrap.End;
    if not (!json = "") 
    then 
    (
        let fname = 
            if s < 0 || s = 100
            then !json ^ ".error"
            else if s = 200
            then !json ^ ".timeout"
            else !json
        in
        Yojson.Safe.to_file fname (`Assoc
            [ 
                ("results", (result_mh#to_json));
                ("stats", (Stats.to_json ()))
            ]);
    )
    ;
    exit s

class mh () =
    object (self)
        inherit Message.handler () as super

        method handle msg =
            match msg.Message.ext_value with
            | Trace.Wrap.TargetCrashed -> fini 1
            | Message.Uncaught(_) -> fini 100
            | Trace.Wrap.GeneralTimeout -> fini 200
            | _ ->
            (
                if Message.is_error msg then Stats.GeneralStats.warn_cnt#incr;
                None
            )
    end
    
let main () =
    Stats.GeneralStats.runtime#start;
    Random.init @@ Int.of_float @@ Unix.time ();
    Symbolic.Binsec_options.init_from_target ();
    p := Policies.Policy.init ();
    Message.Wrap.send (Wrap.Policies);
    Sys.set_signal Sys.sigint (Sys.Signal_handle(fini));
    Sys.set_signal Sys.sigsegv (Sys.Signal_handle(fini));
    Sys.set_signal Sys.sigpipe (Sys.Signal_handle(fini));
    Trace.start ();
    while true
    do
        let trace = Trace.next () in
        match trace with
        | Ans("DONE")
        | Ans("") -> fini 0
        | _ ->
        (
            p := List.map (fun (p, (module P : Policies.Policy.Sig)) -> P.update (fun p -> P.A.analyze trace p) p, (module P : Policies.Policy.Sig)) !p;
            Stats.GeneralStats.cnt#incr
        )
    done

let _ =
    Options.register_option ("-json", Arg.Set_string json, "Output stats and results in json format to specified file.");
    Message.with_handler
        (
            fun () -> 
                let parsed = Message.with_handler Options.parse_args @@ new Message.logger in
                Message.with_handlers
                    (
                        fun () ->
                            Symbolic.Binsec_options.init ();
                            if parsed
                            then Message.with_handler main @@ new Message.cleaner
                            else if Options.has_todos ()
                            then Options.do_todos ()
                            else Message.Wrap.send Wrap.Usage
                    )
                    [(new Message.logger); (result_mh :> Message.handler)]
        )
        @@ new mh ()

