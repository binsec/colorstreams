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

let help_msg = "colorstreams [options] <target>"

let target = ref ""

let desc_prefix = "\n       "

let opt_list = ref []

let pol_opts = ref Utils.StringMap.empty

let dynamic_opt_list = ref []

module TaggedOptions =
    struct
        class ['a] opt (default : 'a) =
            object (self)
                val mutable value = default
                val default = default
                val mutable tagged = Utils.StringMap.empty

                method get ?(tag : string option) =
                    match tag with
                    | Some(tag) -> 
                    (
                        try
                            Utils.StringMap.find tag tagged
                        with Not_found -> value
                    )
                    | None -> value

                method maybe_get ?tag = function
                    | Some(value) -> value
                    | None -> self#get ?tag

                method default = default

                method set ?(tag : string option) v =
                    match tag with
                    | Some(tag) -> tagged <- Utils.StringMap.add tag v tagged
                    | None -> value <- v
            end

        type t = Untagged of (unit -> unit) | Bool of bool opt | String of string opt | Int of int opt

        let registered = ref Utils.StringMap.empty

        let register name opt =
            registered := Utils.StringMap.add name opt !registered

        let to_arg ?(tag = None) = function
            | Untagged(todo) -> Arg.Unit(todo)
            | Bool(opt) -> Arg.Unit(fun () -> opt#set ?tag @@ not opt#default)
            | String(opt) -> Arg.String(fun s -> opt#set ?tag s)
            | Int(opt) -> Arg.Int(fun i -> opt#set ?tag i)

        let def_tag tag =
            if not (tag = "")
            then dynamic_opt_list := Utils.StringMap.fold (fun k opt res -> ("-" ^ tag ^ ":" ^ k, to_arg ~tag:(Some(tag)) opt, "")::res) !registered !dynamic_opt_list
    end

let anon_args_handler anon =
    target := anon

let get_target () =
    !target

let mk_opt_list () =
    (Utils.StringMap.fold (fun k l res -> res @ (((Printf.sprintf "\r      \n---------- %s policy options ----------" k), Arg.Unit(fun () -> ()), "\n")::!l)) !pol_opts !opt_list) @ ["\r        ", Arg.Unit(fun () -> ()), "\n"]

let parse_args () =
    dynamic_opt_list := mk_opt_list ();
    Arg.parse_dynamic dynamic_opt_list anon_args_handler help_msg;
    dynamic_opt_list := [];
    not (!target = "")

let parse_args_argv argv =
    assert (not (!dynamic_opt_list = []));
    Arg.parse_argv_dynamic ~current:(ref 0) argv dynamic_opt_list anon_args_handler help_msg

let pp_help () =
    Arg.usage_string (mk_opt_list ()) help_msg

let add_opt opt l =
    l := match opt with (name, action, msg) -> !l @ [name, action, desc_prefix ^ msg]

let register_policy_option pname name topt desc =
    let opt = ("-" ^ pname ^ name, TaggedOptions.to_arg topt, desc) in
    TaggedOptions.register (pname ^ name) topt;
    try
        let opts = Utils.StringMap.find pname !pol_opts in
        add_opt opt opts
    with Not_found ->
    (
        let opts = ref [] in
        pol_opts := Utils.StringMap.add pname opts !pol_opts;
        add_opt opt opts
    )

let register_option opt =
    add_opt opt opt_list

let add_alias opt alias l =
    let name, action, _ = List.find (fun (name, _, _) -> name = opt) !l in
    add_opt (alias, action, Printf.sprintf "Alias for %s." name) l

let register_alias opt alias =
    add_alias opt alias opt_list

let register_policy_alias pname opt alias =
    let opts = Utils.StringMap.find pname !pol_opts in
    TaggedOptions.register (pname ^ alias) @@ Utils.StringMap.find (pname ^ opt) !TaggedOptions.registered;
    add_alias ("-" ^ pname ^ opt) ("-" ^ pname ^ alias) opts

let todos = ref []

let register_todo todo =
    todos := todo::!todos

let has_todos () =
    not (!todos = [])

let do_todos () =
    List.iter (fun f -> f ()) !todos;
    todos := []

let _ =
    register_option ("-tag", Arg.String(TaggedOptions.def_tag), "Define a new policy tag. Policies can be tagged by adding \":<tag>\" to their name. Options can be set specifically for tagged policies by adding a \"<tag>:\" prefix to them (-policy-option -> -tag:policy-option).")
