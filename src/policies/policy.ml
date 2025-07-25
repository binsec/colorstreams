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

let selected = ref ""

module type BaseSig =
    sig
        type p

        module A : Analysis.Sig with type t = p

        val name : string
        val desc : string
        val init : ?tag:(string option) -> unit -> p
    end

type wrap = ..

module type Sig =
    sig
        include BaseSig

        val wrap : p -> wrap
        val update : (A.t -> A.t) -> wrap -> wrap
        val apply : (A.t -> 'a) -> wrap -> 'a
    end

module Make (Base : BaseSig) =
    struct
        include Base

        type wrap += Wrap of p

        let wrap p =
            Wrap(p)

        let update f = function
            | Wrap(p) -> Wrap(f p)
            | _ -> assert false

        let apply f = function
            | Wrap(p) -> f p
            | _ -> assert false

        let _ =
            A.options name
    end

module NonePolicyBase =
    struct
        module A = Analysis.Make (Analysis.NoneBank) (Analysis.NoneState)

        type p = A.t

        let name = "none"
        let desc = "Prints raw tracer output."

        let init ?(tag = None) () =
            let a = A.create ~tag name in
            Stats.register ~silent:true @@ A.add_stats_to_bundle (Stats.Bundle.create @@ Identifier.pp @@ A.get_name a) a;
            A.TraceCallbacks.add_const "pp trace" (fun line _ -> Message.Wrap.send (Message.Wrap.BigInfo(Trace.pp line))) a
    end

module NonePolicy = Make (NonePolicyBase)

let policies = ref Utils.StringMap.empty

let register_policy (module P : Sig) =
    try
        ignore @@ Utils.StringMap.find P.name !policies;
        raise (Failure "could not register policy %s: name already in use")
    with Not_found -> policies := Utils.StringMap.add P.name (module P : Sig) !policies

let parse s =
    let polexp = Str.regexp {|\([a-zA-Z]+\)\(:\(.+\)\)?|} in
    List.filter_map
        (
            fun p ->
            (
                if Str.string_match polexp p 0
                then
                (
                    let name = Str.matched_group 1 p in
                    let tag =
                        try
                            Some(Str.matched_group 3 p)
                        with Not_found -> None
                    in
                    Some(name, tag)
                )
                else if p = "" then None
                else raise (Failure (Printf.sprintf "invalid policy formatting <%s>" p))
            )
        )
        @@ String.split_on_char ';' s

let get name =
    try
        let module P = (val (Utils.StringMap.find name !policies)) in
        (module P : Sig)
    with Not_found -> raise (Failure (Printf.sprintf "unknown policy <%s>" name))

let init () =
    if !selected = ""
    then
    (
        Message.Wrap.send (Message.Wrap.BigWarning(lazy("No policy specified, defaulting to printing tracer output.")));
        let module P = (val (get "none")) in
        [P.wrap @@ P.init (), (module P : Sig)]
    )
    else List.map 
        (
            fun (name, tag) -> 
                let module P = (val (get name)) in
                P.wrap @@ P.init ~tag (), (module P : Sig)
        )
        @@ parse !selected

let pp () =
    let pp_pol _ (module P : Sig) res =
        Printf.sprintf "%s%-20s%s\n" res ("- " ^ P.name) P.desc
    in
    let msg = Utils.StringMap.fold pp_pol !policies "Available analysis policies:\n" in
    Message.Wrap.send (Message.Wrap.BigInfo(msg))

let _ =
    register_policy (module NonePolicy);
    Options.register_option ("-policy", Arg.Set_string selected, "Select a taint policy (format: policy1;policy2:tag;...).");
    Options.register_option ("-p", Arg.Set_string selected, "Alias for -policy.");
    Options.register_option ("-list-policies", Arg.Unit(fun () -> Options.register_todo pp), "List available policies.")
