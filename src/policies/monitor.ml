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

let policies_opt = new Options.TaggedOptions.opt ""
let monitors_opt = new Options.TaggedOptions.opt ""

module MonitorMap = Utils.StringMap

let monitors = ref MonitorMap.empty

let register name desc (monit : ?tag:(string option) -> unit -> Message.handler) =
    try
        ignore @@ MonitorMap.find name !monitors;
        raise (Failure(Printf.sprintf "monitor name <%s> already taken" name))
    with Not_found -> monitors := MonitorMap.add name (name, desc, monit) !monitors

let list_monitors () =
    let s = MonitorMap.fold (fun _ (name, desc, _) res -> res ^ Printf.sprintf "- %s:\n\t%s\n" name desc) !monitors "Available monitors:\n" in
    Message.Wrap.send (Message.Wrap.BigInfo(s))

let get (tag : string option) name =
    try
        let _, _, monitor = MonitorMap.find name !monitors in
        monitor ~tag ()
    with Not_found -> raise (Failure(Printf.sprintf "No monitor named <%s> available." name))

module PerfMonitor =
    struct
        type Message.signal += Slow of string * Function.CallId.t

        let expected_length_opt = new Options.TaggedOptions.opt (-1)
        let target_time_opt = new Options.TaggedOptions.opt (-1)

        module Stretch =
            struct
                type t =
                    {
                        func : string;
                        call_id : Function.CallId.t;
                        timer : Stats.timer;
                        cnt : Stats.counter
                    }

                let create func call_id =
                    let timer = new Stats.timer "" "" in
                    let cnt = new Stats.counter "" "" 0 in
                    {func; call_id; timer; cnt}

                let create_dummy () =
                    create "???" {Function.CallId.site_id = -1; Function.CallId.stack_id = -1}

                let start s =
                    s.timer#start

                let stop s =
                    s.timer#stop;
                    s.cnt#incr
            end

        module SlowList =
            struct
                type t =
                    {
                        l : (string * float) list;
                        n : int;
                        lim : int
                    }

                let create lim = 
                    {l = []; n = 0; lim}

                let add v score s =
                    let rec aux n v score = function
                        | [] when n = s.lim -> [], false
                        | [] -> [v, score], true
                        | (v_, score_)::t when score > score_ -> 
                        (
                            let l, incr = aux (n + 1) v_ score_ t in
                            (v, score)::l, incr
                        )
                        | h::t ->
                            let l, incr = aux (n + 1) v score t in
                            h::l, incr
                    in
                    let l, incr = aux 0 v score s.l in
                    let n = if incr then s.n + 1 else s.n in
                    {s with l; n}
            end

        type entry =
            {
                current : Stretch.t list;
                warned_cool : Stats.timer;
                timer : Stats.timer;
                cnt : Stats.counter;
                per_func : (float * int) Utils.StringMap.t;
                expected : int option;
                target : int option
            }

        let mk_entry expected target func call_id =
            let current = [Stretch.create func call_id] in
            let warned_cool = new Stats.timer "" "" in
            let timer = new Stats.timer "" "" in
            let cnt = new Stats.counter "" "" 0 in
            let per_func = Utils.StringMap.empty in
            {current; warned_cool; timer; cnt; per_func; expected; target}

        let throughput time cnt =
            (float_of_int cnt) /. time

        let avg_avg_per_func e =
            let sum, div = Utils.StringMap.fold (fun _ (time, cnt) (sum, div) -> sum +. throughput time cnt, div + 1) e.per_func (Float.zero, 0) in
            if div > 0 then sum /. (float_of_int div) else 1.0

        let check_target e =
            match e.target, e.expected with
            | Some(target), Some(expect) ->
            (
                let curr_thru = throughput e.timer#get e.cnt#get in
                let target_thru = throughput (float_of_int target) expect in
                target_thru < curr_thru
            )
            | _ -> false

        let check_stuck curr_time curr_cnt e =
            if (not @@ check_target e) && curr_time /. e.timer#get > 0.05
            then
            (
                let curr = throughput curr_time curr_cnt in
                let avg = avg_avg_per_func e in
                if curr /. avg < 0.001
                then
                (
                    Message.Wrap.send (Message.Wrap.Debug("MONITORING", lazy (Printf.sprintf "Being slow in <%s>: %.3g instr / s" (List.hd e.current).Stretch.func curr)));
                    Message.send_signal (Slow((List.hd e.current).func, (List.hd e.current).call_id));
                    true
                )
                else false
            )
            else false

        let new_current func call_id e =
            let current = (Stretch.create func call_id)::e.current in
            e.warned_cool#reset;
            {e with current}

        let end_current call_id e =
            let rec aux e =
                let time, cnt =
                    try
                        Utils.StringMap.find (List.hd e.current).Stretch.func e.per_func
                    with Not_found -> 0.0, 0
                in
                let time = time +. (List.hd e.current).timer#get in
                let cnt = cnt + (List.hd e.current).cnt#get in
                ignore @@ check_stuck time cnt e;
                let per_func = Utils.StringMap.add (List.hd e.current).func (time, cnt) e.per_func in
                e.warned_cool#reset;
                match e.current with
                | h::t when h.Stretch.call_id = call_id -> {e with current = t; per_func}
                | h::[] -> {e with current = [Stretch.create_dummy ()]; per_func}
                | h::t -> aux {e with current = t; per_func}
                | [] -> assert false
            in
            match e.current with
            | [] -> {e with current = [Stretch.create_dummy ()]}
            | _ -> aux e 

        let entry_start instr e =
            Stretch.start @@ List.hd e.current;
            e.timer#start;
            e.warned_cool#start;
            e

        let entry_stop instr e =
            e.timer#stop;
            e.warned_cool#stop;
            Stretch.stop @@ List.hd e.current;
            e.cnt#incr;
            if e.warned_cool#get > 10.0
            then
            (
                if check_stuck (List.hd e.current).timer#get (List.hd e.current).cnt#get e
                then e
                else 
                (
                    e.warned_cool#reset;
                    e
                )
            )
            else e

        class monitor expected target =
            object (self)
                inherit Message.handler () as super

                val mutable state = Utils.StringMap.empty

                method handle msg =
                    let sender = lazy (Identifier.pp @@ Message.get_sender msg) in
                    let entry = lazy (Utils.StringMap.find (Lazy.force sender) state) in
                    match msg.Message.ext_value with
                    | Analysis.Monitoring.Monit(Analysis.Monitoring.Func(Function.Entry({fname; call_id = Some(call_id); _})), Analysis.Monitoring.Start) ->
                    (
                        let entry =
                            try
                                new_current fname call_id @@ Lazy.force entry
                            with Not_found -> mk_entry expected target fname call_id
                        in
                        state <- Utils.StringMap.add (Lazy.force sender) entry state;
                        super#handle msg
                    )
                    | Analysis.Monitoring.Monit(Analysis.Monitoring.Func(Function.CallRet(call_id, _)), Analysis.Monitoring.Start) ->
                    (
                        try
                            let entry = end_current call_id @@ Lazy.force entry in
                            state <- Utils.StringMap.add (Lazy.force sender) entry state;
                            super#handle msg
                        with Not_found -> super#handle msg
                    )
                    | Analysis.Monitoring.Monit(Analysis.Monitoring.Ins(instr), status) ->
                    (
                        try
                            let entry = Lazy.force entry in
                            let entry =
                                match status with
                                | Analysis.Monitoring.Start -> entry_start instr entry
                                | Analysis.Monitoring.Done -> entry_stop instr entry
                                | _ -> entry
                            in
                            state <- Utils.StringMap.add (Lazy.force sender) entry state;
                            super#handle msg
                        with Not_found -> super#handle msg
                    )
                    | Analysis.Monitoring.Monit(Analysis.Monitoring.Fini, Analysis.Monitoring.Done) ->
                    (
                        try
                            let entry = end_current (Function.CallId.create 0 0) @@ Lazy.force entry in
                            Message.Wrap.send (Message.Wrap.BigInfo(Printf.sprintf "Performance monitoring:\n\t%.3g instr / s avg.\n\t%.3g instr / s avg. per function" (throughput entry.timer#get entry.cnt#get) @@ avg_avg_per_func entry));
                            super#handle msg
                        with Not_found -> super#handle msg
                    )
                    | _ -> super#handle msg
            end

        let mk_monitor ?(tag = None) () =
            let expected = expected_length_opt#get ?tag in
            let expected = if expected < 0 then None else Some(expected) in
            let target = target_time_opt#get ?tag in
            let target = if target < 0 then None else Some(target) in
            new monitor expected target

        let _ = 
            register "performance" "Monitors analysis policy instruction throughput." mk_monitor;
            Options.register_policy_option "monitor-performance" "-expected-length" (Options.TaggedOptions.Int(expected_length_opt)) "Expected trace length (number of instructions). Must also set -monitor-performance-target-runtime.";
            Options.register_policy_option "monitor-performance" "-target-runtime" (Options.TaggedOptions.Int(target_time_opt)) "Targeted runtime (seconds). Must also set -monitor-performance-expected-length."
    end

class mh =
    object(self)
        inherit Message.handler () as super

        method handle msg =
            match msg.Message.ext_value with
            | Analysis.Monitoring.Monit(_) -> None
            | _ -> super#handle msg
    end

module S =
    struct
        type t =
            {
                policies : (Policy.wrap * (module Policy.Sig)) list;
                monitors : Message.handler list;
                mh : mh;
            }

        let create ?(tag = None) () =
            {policies = []; monitors = []; mh = new mh}

        let init_policies p =
            List.map
                (
                    fun (name, tag) ->
                        let module P = (val (Policy.get name)) in
                        P.wrap @@ P.init ~tag (), (module P : Policy.Sig)
                )
                p

        let set_policies policies s =
            {s with policies}

        let with_monitors f s =
            Message.with_handlers f (s.monitors @ [s.mh])

        let update_policies f s =
            let aux () =
                let policies = List.map (fun (p, (module P : Policy.Sig)) -> (f p (module P : Policy.Sig)), (module P : Policy.Sig)) s.policies in
                {s with policies}
            in
            with_monitors aux s

        let set_monitors tag monitors s =
            let monitors = List.map (get tag) monitors in
            {s with monitors}

        let add_stats_to_bundle b _ = b
    end

module A = Analysis.Make (Analysis.NoneBank) (S)

let callback trace a =
    A.set_state a @@ S.update_policies (fun p (module P : Policy.Sig) -> P.update (fun a -> P.A.analyze trace a) p) @@ A.get_state a

let end_callback signal a =
    A.set_state a @@ S.update_policies (fun p (module P : Policy.Sig) -> P.update (fun a -> P.A.fini signal a) p) @@ A.get_state a

let init_analysis
    ?policies
    ?monitors
    ?(tag = None)
        name =
    let policies =
        match policies with
        | Some(policies) -> policies
        | None -> S.init_policies @@ Policy.parse @@ policies_opt#get ?tag
    in
    let monitors =
        match monitors with
        | Some(monitors) -> monitors
        | None -> String.split_on_char ';' @@ monitors_opt#get ?tag
    in
    let monitors = if monitors = [""] then [] else monitors in
    let a = A.create ~ignore_sources:true ~ignore_sinks:true ~tag name in
    let a = a
        |> A.get_state
        |> S.set_policies policies
        |> S.set_monitors tag monitors
        |> A.set_state a
    in
    let todo a =
        a
            |> A.TraceCallbacks.add "analysis" callback
            |> A.EndCallbacks.add "end" end_callback
    in
    A.FunctionCallbacks.add "monitor start" (A.FunctionCallbacks.start_callback ~name:"monitor start" ~todo ~add_sink_checks:false ~add_source_checks:false) a

module PB =
    struct
        module A = A
        
        type p = A.t

        let name = "monitor"
        let desc = "Monitor underlying analysis policies."

        let init ?(tag = None) () =
            init_analysis ~tag "Monitor"
    end

module P = Policy.Make (PB)

let _ =
    Policy.register_policy (module P);
    Options.register_policy_option P.name "-policies" (Options.TaggedOptions.String(policies_opt)) "Policies under monitoring, enabled with the corresponding -<policy>-monitor (format: policy1;policy2:tag;...).";
    Options.register_policy_alias P.name "-policies" "-p";
    Options.register_policy_option P.name "-monitors" (Options.TaggedOptions.String(monitors_opt)) "Select monitors (format: monitor1;monitor2...).";
    Options.register_policy_alias P.name "-monitors" "-m";
    Options.register_policy_option P.name "-list-monitors" (Options.TaggedOptions.Untagged(fun () -> Options.register_todo list_monitors)) "List available monitors."
