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

let producers_opt = new Options.TaggedOptions.opt ""
let consumers_opt = new Options.TaggedOptions.opt ""
let allow_ext_src_opt = new Options.TaggedOptions.opt true
let allow_ext_snk_opt = new Options.TaggedOptions.opt true

class catcher =
    object (self)
        inherit Message.handler () as super

        val mutable srcs = []
        val mutable snks = []

        method handle msg =
            let _ =
                match msg.Message.ext_value with
                | Analysis.Wrap.Src(src) -> srcs <- src::srcs
                | Analysis.Wrap.Snk(snk) -> snks <- snk::snks
                | _ -> ()
            in
            super#handle msg

        method get_srcs =
            let res = List.rev srcs in
            srcs <- [];
            res

        method get_snks =
            let res = List.rev snks in
            snks <- [];
            res
    end

module S =
    struct
        type t =
            {
                producers : (Policy.wrap * (module Policy.Sig)) list;
                consumers : (Policy.wrap * (module Policy.Sig)) list;
                allow_ext_src : bool;
                allow_ext_snk : bool;
                mh : catcher;
            }

        let create ?(tag = None) () =
            {producers = []; consumers = []; allow_ext_src = true; allow_ext_snk = true; mh = new catcher}

        let init_policies p =
            List.map
                (
                    fun (name, tag) ->
                        let module P = (val (Policy.get name)) in
                        P.wrap @@ P.init ~tag (), (module P : Policy.Sig)
                )
                p

        let init_producers p s =
            {s with producers = init_policies p}

        let init_consumers p s =
            {s with consumers = init_policies p}

        let set_producers producers s =
            {s with producers}

        let set_consumers consumers s =
            {s with consumers}

        let set_allow_ext_src allow_ext_src s =
            {s with allow_ext_src}

        let set_allow_ext_snk allow_ext_snk s =
            {s with allow_ext_snk}

        let update_policies f p =
            List.map (fun (p, (module P : Policy.Sig)) -> (f p (module P : Policy.Sig)), (module P : Policy.Sig)) p

        let update_producers f s =
            {s with producers = update_policies f s.producers}

        let update_consumers f s =
            {s with consumers = update_policies f s.consumers}

        let add_stats_to_bundle b _ = b
    end

module A = Analysis.Make (Analysis.NoneBank) (S)

type t = A.t

let callback trace a =
    let s = A.get_state a in
    let s = Message.with_handler (fun () -> S.update_producers (fun p (module P : Policy.Sig) -> P.update (fun a -> P.A.analyze trace a) p) s) (s.mh :> Message.handler) in
    let s = List.fold_left 
        (
            fun s src -> 
                let src = Trace.Src(src) in
                if trace = src (*will be transmitted when updating consumers with trace*)
                then s
                else S.update_consumers (fun p (module P : Policy.Sig) -> P.update (fun a -> P.A.analyze src a) p) s
        ) 
        s s.mh#get_srcs 
    in
    let s = List.fold_left 
        (
            fun s snk -> 
                let snk = Trace.Snk(snk) in
                if trace = snk (*idem*)
                then s
                else S.update_consumers (fun p (module P : Policy.Sig) -> P.update (fun a -> P.A.analyze snk a) p) s
        ) 
        s s.mh#get_snks 
    in
    match trace, s.allow_ext_src, s.allow_ext_snk with
    | Trace.Src(_), false, _ 
    | Trace.Snk(_), _, false -> A.set_state a s
    | _ -> A.set_state a @@ S.update_consumers (fun p (module P : Policy.Sig) -> P.update (fun a -> P.A.analyze trace a) p) s


let end_callback signal a =
    let s = A.get_state a in
    let s = S.update_producers (fun p (module P : Policy.Sig) -> P.update (fun a -> P.A.fini signal a) p) s in
    A.set_state a @@ S.update_consumers (fun p (module P : Policy.Sig) -> P.update (fun a -> P.A.fini signal a) p) s

let init_analysis 
    ?producers 
    ?consumers
    ?allow_ext_src
    ?allow_ext_snk
    ?(tag = None)
        name =
    let producers = 
        match producers with
        | Some(producers) -> producers
        | None -> S.init_policies @@ Policy.parse @@ producers_opt#get ?tag 
    in
    let consumers = 
        match consumers with
        | Some(consumers) -> consumers
        | None -> S.init_policies @@ Policy.parse @@ consumers_opt#get ?tag
    in
    let allow_ext_src = allow_ext_src_opt#maybe_get ?tag allow_ext_src in
    let allow_ext_snk = allow_ext_snk_opt#maybe_get ?tag allow_ext_snk in
    let a = A.create ~ignore_sources:true ~ignore_sinks:true ~tag name in
    let a = a
        |> A.get_state
        |> S.set_producers producers
        |> S.set_consumers consumers
        |> S.set_allow_ext_src allow_ext_src
        |> S.set_allow_ext_snk allow_ext_snk
        |> A.set_state a
    in
    let todo a =
        a
            |> A.TraceCallbacks.add "analysis" callback
            |> A.EndCallbacks.add "end" end_callback
    in
    A.FunctionCallbacks.add "composition start" (A.FunctionCallbacks.start_callback ~name:"composition start" ~todo ~add_sink_checks:false ~add_source_checks:false) a

module PB =
    struct
        module A = A

        type p = A.t

        let name = "compose"
        let desc = "Send sources and sinks from producers to consumers."

        let init ?(tag = None) () =
            init_analysis ~tag "Compose"
    end

module P = Policy.Make (PB)

let _ =
    Policy.register_policy (module P);
    Options.register_policy_option "compose" "-producers" (Options.TaggedOptions.String(producers_opt)) "Source / sink producers (format: policy1;policy2:tag;...).";
    Options.register_policy_alias "compose" "-producers" "-p";
    Options.register_policy_option "compose" "-consumers" (Options.TaggedOptions.String(consumers_opt)) "Source / sink consumers (format: policy1;policy2:tag;...).";
    Options.register_policy_alias "compose" "-consumers" "-c";
    Options.register_policy_option "compose" "-no-ext-src" (Options.TaggedOptions.Bool(allow_ext_src_opt)) "Do not transmit external sources to consumers (not produced by producers).";
    Options.register_policy_option "compose" "-no-ext-snk" (Options.TaggedOptions.Bool(allow_ext_snk_opt)) "Do not transmit external sinks to consumers (not produced by producers)."
