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

let concrete_w_opt = new Options.TaggedOptions.opt ""
let properties_opt = new Options.TaggedOptions.opt ""
let wc_native_opt = new Options.TaggedOptions.opt false
let force_opt = new Options.TaggedOptions.opt false
let crwa_opt = new Options.TaggedOptions.opt false
let saww_opt = new Options.TaggedOptions.opt 16
let dry_opt = new Options.TaggedOptions.opt false
let src_bytes_opt = new Options.TaggedOptions.opt ""
let cull_opt = new Options.TaggedOptions.opt (-1)

let parse_props s =
    if s = ""
    then []
    else List.fold_right
        (
            fun e res ->
                try
                    (Symbolic.Properties.Property.get e)::res
                with Not_found ->
                (
                    Message.Wrap.send (Message.Wrap.BigWarning(lazy(Printf.sprintf "unknown property <%s>" e)));
                    res
                )
        )
        (String.split_on_char ';' s)
        []

module S =
    struct
        type t =
            {
                properties : Symbolic.Properties.Property.t list;
                wc_native : bool;
                force : bool;
                cull_after : int;
                culled : bool Utils.StringMap.t
            }

        let create ?(tag = None) () =
            {properties = []; wc_native = false; force = false; cull_after = (-1); culled = Utils.StringMap.empty}

        let add_stats_to_bundle b _ = b

        let set_properties properties s =
            {s with properties}

        let set_wc_native wc_native s =
            {s with wc_native}
            
        let set_force force s =
            {s with force}

        let set_cull_after cull_after s =
            {s with cull_after}

        let get_force_props s =
            s.force

        let get_properties s =
            s.properties

        let get_wc_native s =
            s.wc_native

        let check_force snk positive s =
            positive || s.force
    end

module SS = Symbolic.State.Make ()
module SB = Symbolic.SymbolicBank.Make (SS)
module SA = Analysis.Make (SB) (S)

let parse_src_bytes s =
    let s, action = 
        if String.starts_with ~prefix:"only!" s 
        then String.sub s 5 ((String.length s) - 5), SB.Only 
        else s, SB.Restrict 
    in
    if s = ""
    then action, Utils.StringMap.empty
    else
    (
        let rangeexp = Str.regexp {|\([0-9]+\)-\([0-9]+\)|} in
        action,
        List.fold_left
            (
                fun res e ->
                    try
                        match String.split_on_char ':' e with
                        | [src; ""]
                        | [src] -> Utils.StringMap.add src [] res
                        | [src; blist] ->
                        (
                            let blist = List.map
                                (
                                    fun e ->
                                        if Str.string_match rangeexp e 0
                                        then
                                        (
                                            let lo = int_of_string @@ Str.matched_group 1 e in
                                            let hi = int_of_string @@ Str.matched_group 2 e in
                                            SB.Range(lo, hi)
                                        )
                                        else SB.Single(int_of_string e)
                                )
                                @@ String.split_on_char ',' blist
                            in
                            Utils.StringMap.add src blist res
                        )
                        | _ -> assert false
                    with e -> raise (Failure (Printf.sprintf "source symbolic byte selection: invalid formatting (%s)" @@ Printexc.to_string e))
            )
            Utils.StringMap.empty
            @@ String.split_on_char ';' s
    )

module LibFunStubsBase =
    struct
        let modulename = "libfun"

        type state = SA.t
        type action = Update of state | OnRet of string * (Function.func list -> state -> state) | Both of state * string * (Function.func list -> state -> state) | Nothing
        type kind = Any

        let pp_kind _ =
            "stub"

        let check_kind _ call_id a _ =
            match SB.get_mode @@ SA.get_bank a with
            | SB.Symbolic -> a, true
            | _ -> a, false

        let act _ call_id a action = 
            let a = 
                match action with
                | Update(a) -> a
                | OnRet(name, cb) -> SA.FunctionCallbacks.on_callret call_id name cb a
                | Both(a, name, cb) -> SA.FunctionCallbacks.on_callret call_id name cb a
                | Nothing -> a
            in
            SA.set_bank a @@ SB.switch_mode (SB.Concrete_until(call_id)) @@ SA.get_bank a
    end

module LibFunStubs =
    struct
        include Stubs.Make (LibFunStubsBase)

        module Default_stubs =
            struct
                let get_arg_or_ret_val n func =
                    if n < 0
                    then lazy (Trace.stoval @@ Function.get_iret 0 func)
                    else
                    (
                        let value = Trace.stoval @@ Function.get_iarg n func in
                        lazy value
                    )

                let three_argidx_parsr stub = function
                    | [itgt; isrc; isize] ->
                    (
                        let itgt = int_of_string itgt in
                        let isrc = int_of_string isrc in
                        let isize = int_of_string isize in
                        stub itgt isrc isize
                    )
                    | _ -> raise (Invalid_argument("wrong number of arguments"))

                let generic_memcpy_name = "memcpy"

                let generic_memcpy_spec = ["tgt arg number"; "src arg number"; "size arg number"]

                let generic_memcpy_desc = "copy <size> bytes from <src> to <tgt> (no overlap)"

                let generic_memcpy itgt isrc isize func _ a =
                    let size = Z.to_int @@ Trace.stoval @@ Function.get_iarg isize func in
                    if size = 0
                    then LibFunStubsBase.Nothing
                    else
                    (
                        let tgt = Storage.Memory(Trace.stoval @@ Function.get_iarg itgt func, size) in
                        let src = Storage.Memory(Trace.stoval @@ Function.get_iarg isrc func, size) in
                        let do_memcpy _ a =
                            SA.set_bank a @@ SB.assign_expr tgt (Expr.Var(src)) @@ SA.get_bank a
                        in
                        LibFunStubsBase.OnRet("memcpy", do_memcpy)
                    )

                let generic_memcpy_parsr = three_argidx_parsr generic_memcpy

                let generic_memmove_name = "memmove"

                let generic_memmove_spec = generic_memcpy_spec

                let generic_memmove_desc = "copy <size> bytes from <src> to <tgt> (can overlap)"

                let memmove_cnt = ref 0

                let generic_memmove itgt isrc isize func _ a =
                    let size = Z.to_int @@ Trace.stoval @@ Function.get_iarg isize func in
                    if size = 0
                    then LibFunStubsBase.Nothing
                    else
                    (
                        let tgt = Storage.Memory(Trace.stoval @@ Function.get_iarg itgt func, size) in
                        let src = Storage.Memory(Trace.stoval @@ Function.get_iarg isrc func, size) in
                        let savesto = Storage.Custom(Printf.sprintf "memmove_save_%d" !memmove_cnt, size, Trace.stoval src) in
                        memmove_cnt := !memmove_cnt + 1;
                        let a = SA.set_bank a @@ SB.assign_expr savesto (Expr.Var(src)) @@ SA.get_bank a in
                        let do_memmove _ a =
                            SA.set_bank a @@ SB.assign_expr tgt (Expr.Var(savesto)) @@ SA.get_bank a
                        in
                        LibFunStubsBase.Both(a, "memmove", do_memmove)
                    )

                let generic_memmove_parsr = three_argidx_parsr generic_memmove

                let generic_memset_name = "memset"

                let generic_memset_spec = ["tgt arg number"; "pattern arg number"; "size arg number"]

                let generic_memset_desc = "set <size> bytes of <tgt> to <pattern>"

                let generic_memset itgt ipattern isize func _ a =
                    let size = Z.to_int @@ Trace.stoval @@ Function.get_iarg isize func in
                    let tgt = Trace.stoval @@ Function.get_iarg itgt func in
                    let pattern = Function.get_iarg ipattern func in
                    let pattexp = Expr.Unop(Expr.Restrict(0, 0), Expr.Var(pattern)) in
                    let do_memset _ a =
                        SA.set_bank a
                            @@ List.fold_left (fun b sto -> SB.assign_expr sto pattexp b) (SA.get_bank a)
                            @@ List.init size (fun i -> Storage.Memory(Address.add_int tgt i, 1))
                    in
                    LibFunStubsBase.OnRet("memset", do_memset)

                let generic_memset_parsr = three_argidx_parsr generic_memset

                let _ =
                    (*memcpy & co*)
                    register generic_memcpy_name generic_memcpy_spec generic_memcpy_desc LibFunStubsBase.Any generic_memcpy_parsr;
                    register_default "memcpy" "default_memcpy" Any (generic_memcpy 0 1 2);
                    register_default "memcpy@plt" "default_memcpy" Any (generic_memcpy 0 1 2);
                    register generic_memmove_name generic_memmove_spec generic_memmove_desc LibFunStubsBase.Any generic_memmove_parsr;
                    register_default "memmove" "default_memmove" Any (generic_memmove 0 1 2);
                    register_default "memmove@plt" "default_memmove" Any (generic_memmove 0 1 2);
                    register generic_memset_name generic_memset_spec generic_memset_desc Any generic_memset_parsr;
                    register_default "memset" "default_memset" Any (generic_memset 0 1 2);
                    register_default "memset@plt" "default_memset" Any (generic_memset 0 1 2)
            end
    end

let to_file name s =
    let file = File.create ~keep:true name ".smt2" in
    File.write s file;
    file

let check_properties snk a =
    match SB.get_sink_formula snk @@ SA.get_bank a with
    | Some(proj, sf) ->
    (
        if Sink.has_constr snk
        then Message.Wrap.send (Analysis.Wrap.SinkResult(snk, Result.Generic(Result.mk_string ~label:"Constraint" "sat")))
        ;
        let check p =
            let res =
                if p.Symbolic.Properties.Property.name = "WeakControl" && (SA.get_state a).wc_native
                then
                (
                    let solver = new Stats.sstat "solver" "Solver" in
                    solver#set "bitwuzla";
                    let timer = new Stats.timer "runtime" "Runtime" in
                    timer#start;
                    let _, positive = SB.sink snk @@ SA.get_bank a in
                    timer#stop;
                    let stats = 
                        let stats = Stats.Bundle.create ""
                            |> Stats.Bundle.add_stat (solver :> Stats.stat)
                            |> Stats.Bundle.add_stat (timer :> Stats.stat)
                        in
                        Some(stats)
                    in
                    Result.Generic(Result.mk_bool ~label:"WeakControl" ~stats positive)
                )
                else Symbolic.Properties.Property.PropRes.wrap @@ p.Symbolic.Properties.Property.check proj sf
            in
            Message.Wrap.send (Analysis.Wrap.SinkResult(snk, res))
        in
        List.iter check (SA.get_state a).properties
    )
    | None -> Message.Wrap.send (Analysis.Wrap.SinkResult(snk, Result.Generic((Result.mk_string ~label:"Constraint" "unsat"))))

let print_smt_callback snk a =
    match SB.get_sink_formula snk @@ SA.get_bank a with
    | Some(_, sf) ->
    (
        ignore @@ to_file ((Identifier.pp_basic snk.Sink.name) ^ "_general_state") @@ SB.S.pp_formula @@ SB.get_state @@ SA.get_bank a;
        ignore @@ to_file ((Identifier.pp_basic snk.Sink.name) ^ "_simplified") @@ Symbolic.StateFormula.pp_base sf;
        ignore @@ to_file ((Identifier.pp_basic snk.Sink.name) ^ "_processed") @@ Symbolic.StateFormula.pp sf
    )
    | _ -> ()

let update_instruction i a =
    SA.set_bank a @@ SB.next_instruction i @@ SA.get_bank a

let update_function f a =
    SA.set_bank a @@ SB.next_function f @@ SA.get_bank a

let cull signals a =
    let s = SA.get_state a in
    let b = SA.get_bank a in
    let s, b = 
        if s.S.cull_after >= 0 && SB.get_runtime b > float_of_int s.S.cull_after
        then List.fold_left 
            (
                fun (s, b) signal ->
                    match signal with
                    | Monitor.PerfMonitor.Slow(func, call_id) -> 
                    (
                        try
                            ignore @@ Utils.StringMap.find func s.S.culled;
                            s, b
                        with Not_found ->
                        (
                            let culled = Utils.StringMap.add func true s.culled in
                            Message.Wrap.send (Message.Wrap.Warning(lazy (Printf.sprintf "Concretizing writes in <%s> from now on." func)));
                            {s with culled}, SB.add_concrete func @@ SB.switch_mode (SB.Concrete_until(call_id)) b
                        )
                    )
                    | _ -> s, b
            )
            (s, b) signals
        else s, b
    in
    SA.set_bank (SA.set_state a s) b

let init_analysis 
    ?(register_stats = true)
    ?concrete_w 
    ?properties
    ?src_bytes
    ?wc_native
    ?force
    ?crwa
    ?cull_after
    ?saww
    ?stubs
    ?dry
    ?(tag = None)
        name =
    let concrete_w = concrete_w_opt#maybe_get ?tag concrete_w in
    let properties =
        match properties with
        | Some(properties) -> properties
        | None -> parse_props @@ properties_opt#get ?tag
    in
    let src_bytes_action, src_bytes =
        match src_bytes with
        | Some(action, src_bytes) -> action, src_bytes
        | None -> parse_src_bytes @@ src_bytes_opt#get ?tag
    in
    let wc_native = wc_native_opt#maybe_get ?tag wc_native in
    let force = force_opt#maybe_get ?tag force in
    let crwa = crwa_opt#maybe_get ?tag crwa in
    let cull_after = cull_opt#maybe_get ?tag cull_after in
    let saww = saww_opt#maybe_get ?tag saww in
    let stubs =
        match stubs with
        | Some(stubs) -> stubs
        | None -> LibFunStubs.create @@ LibFunStubs.cli_specs#get ?tag
    in
    let dry = dry_opt#maybe_get ?tag dry in
    let a = SA.create ~tag name in
    let a = SA.set_bank a @@ SB.set_src_bytes ~action:src_bytes_action src_bytes @@ SB.set_crwa crwa @@ SB.set_saww saww @@ SB.set_concrete (String.split_on_char ';' concrete_w) @@ SA.get_bank a in
    let a = SA.get_state a
        |> S.set_properties properties
        |> S.set_wc_native wc_native
        |> S.set_force force
        |> S.set_cull_after cull_after
        |> SA.set_state a
    in
    let a = SA.set_ignore_sources dry @@ SA.set_filter_analyzed_sinks S.check_force a in
    if register_stats
    then Stats.register ~silent:true @@ SA.add_stats_to_bundle (Stats.Bundle.create @@ Identifier.pp @@ SA.get_name a) a
    ;
    let todo a =
        a
            |> SA.SinkCallbacks.add_const "sink smt pp" print_smt_callback
            |> SA.SinkCallbacks.add_const "property checks" check_properties
            |> SA.InstructionCallbacks.add "symbolic update" update_instruction
            |> SA.FunctionCallbacks.add "stubs" ~priority:100 (LibFunStubs.run stubs)
            |> SA.FunctionCallbacks.add "symbolic func update" update_function
            |> SA.SignalCallbacks.add "slow functions culling" cull
    in
    let name = "symbolic start" in
    SA.FunctionCallbacks.add name (SA.FunctionCallbacks.start_callback ~name ~todo) a

module PB =
    struct
        module A = SA

        type p = SA.t

        let name = "symbolic"
        let desc = "Symbolic execution."

        let init ?(tag = None) () =
            init_analysis ~tag "Symbolic Policy"
    end

module P = Policy.Make (PB)

let () =
    Policy.register_policy (module P);
    Options.register_policy_option "symbolic" "-concrete-w" (Options.TaggedOptions.String(concrete_w_opt)) "Concretize write operations for the specified functions (format: name;<regex>;...).";
    Options.register_policy_alias "symbolic" "-concrete-w" "-cw";
    Options.register_policy_option "symbolic" "-properties" (Options.TaggedOptions.String(properties_opt)) "Check properties on sink (format: p1;p2;...).";
    Options.register_policy_alias "symbolic" "-properties" "-p";
    Options.register_policy_option "symbolic" "-wc-native" (Options.TaggedOptions.Bool(wc_native_opt)) "Use native formula to check weak control.";
    Options.register_policy_option "symbolic" "-fp" (Options.TaggedOptions.Bool(force_opt)) "Force checking properties and pmc on custom sinks even if concrete.";
    Options.register_policy_option "symbolic" "-crwa" (Options.TaggedOptions.Bool(crwa_opt)) "Concretize R/W addresses.";
    Options.register_policy_option "symbolic" "-saww" (Options.TaggedOptions.Int(saww_opt)) "Set width for the restriction window of symbolic addresses (default: 16, in both directions. Set to -1 to disable.). Warning: large values may mess with Binsec's formula simplification.";
    Options.register_policy_option "symbolic" "-src-bytes" (Options.TaggedOptions.String(src_bytes_opt)) "Make only specific bytes from named source symbolic (format: src_name:b1,b2-b3,...;... prefix with \"only!\" to ignore unspecified sources).";
    LibFunStubs.register_options "symbolic";
    Options.register_policy_option "symbolic" "-dry" (Options.TaggedOptions.Bool(dry_opt)) "Ignore all sources (fully concrete execution).";
    Options.register_policy_option P.name "-cull-after" (Options.TaggedOptions.Int(cull_opt)) "Start concretizing writes in slow functions after the specified number of seconds (only works when under monitoring with the \"performance\" monitor)."
