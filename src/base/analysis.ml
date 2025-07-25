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

let selected_sinks = ref @@ Str.regexp ".*"
let selected_sources = ref @@ Str.regexp ".*"
let sos = ref @@ Str.regexp "$"
let sops = ref false
let saf = ref @@ Str.regexp "$"

module Wrap =
    struct
        type Result.custom_result += 
            | SinkRes of Sink.t * Result.wrapped_result
            | SmallSinkRes of Sink.t * Result.wrapped_result

        type Message.ext_value += 
            | Snk of Sink.t
            | Src of Source.t
            | Ins of Instruction.t
            | Func of Function.t
            | WLoc of Trace.Gdb.LocRes.result * Message.ext_value
        
        type Message.Wrap.wrap += 
            | SinkResult of Sink.t * Result.wrapped_result
            | SmallSinkResult of Sink.t * Result.wrapped_result
            | Source of Source.t
            | Sink of Sink.t
            | Instruction of Instruction.t
            | Function of Function.t
            | WithLoc of Message.Wrap.wrap

        let unwrapper next = function
            | SinkResult(snk, res) -> 0, false, "RESULT", 
                (Result.Result(Result.Custom((SinkRes(snk, res)), lazy (Result.to_generic res)))), 
                lazy (Result.pp ~pstats:(!Message.verbosity > 0) @@ Result.to_generic res)
            | SmallSinkResult(snk, res) -> 2, false, "RESULT", 
                (Result.Result(Result.Custom((SmallSinkRes(snk, res)), lazy (Result.to_generic res)))), 
                lazy (Result.pp ~pstats:(!Message.verbosity > 0) @@ Result.to_generic res)
            | Source(src) -> 1, false, "SOURCE", (Src(src)), lazy (Source.pp src)
            | Sink(snk) -> 1, false, "SINK", (Snk(snk)), lazy (Sink.pp snk)
            | Instruction(i) -> 3, false, "INSTRUCTION", (Ins(i)), lazy (Instruction.pp i)
            | Function(f) -> 3, false, "FUNCTION", (Func(f)), lazy (Function.pp f)
            | WithLoc(w) ->
            (
                let verb, err, pref, ext, v = Message.Wrap.unwrap w in
                let loc = Trace.Gdb.get_location @@ Trace.get_gdb () in
                let ext, v =
                    match loc with
                    | Some(loc) -> (WLoc(loc, ext)), lazy ((Lazy.force v) ^ "\n" ^ (Result.pp @@ Trace.Gdb.LocRes.export loc))

                    | _ -> ext, v
                in
                verb, err, pref, ext, v
            )
            | w -> next w 
        
        let _ = Message.Wrap.register_unwrapper unwrapper
    end

module Monitoring =
    struct
        type kind = ..

        type kind +=
            | Trc of Trace.trace
            | Snk of Sink.t
            | Src of Source.t
            | Ins of Instruction.t
            | Func of Function.t
            | Fini

        type status = ..

        type status += Start | Done

        type t = kind * status

        type Message.ext_value += Monit of t

        type Message.Wrap.wrap += Monitoring of t

        let unwrapper next = function
            | Monitoring(kind, status) -> 666, false, "MONITORING", Monit(kind, status), lazy "monitoring"
            | w -> next w

        let with_monit kind f =
            Message.Wrap.send (Monitoring(kind, Start));
            let res = f () in
            Message.Wrap.send (Monitoring(kind, Done));
            res

        let _ = Message.Wrap.register_unwrapper unwrapper
    end

class mh ?(min_verb = 0) name = 
    object (self)
        inherit Message.backlogged_handler ~name:(Some(name)) () as super

        val mutable min_verb = min_verb

        method handle msg =
            let msg =
                match msg.Message.ext_value, msg.Message.handled_by with
                | Stats.Wrap.ExtStats(_), _::_ -> {msg with verbosity = 2}
                | _ -> msg
            in
            match msg.Message.error with
            | Some(_) -> super#handle msg
            | _ ->
            (
                if msg.Message.verbosity < min_verb
                then super#handle {msg with verbosity = min_verb}
                else super#handle msg
            )

        method set_min_verb v =
            min_verb <- v
    end

class sinkres_catcher snk =
    object(self)
        inherit Message.handler () as super

        val mutable results = []
        val mutable smallresults = []

        method handle msg =
            match msg.Message.ext_value with
            | Result.Result(Result.Custom(Wrap.SinkRes(snk_, res), _)) -> 
                if Sink.equals snk snk_
                then 
                (
                    results <- res::results;
                    None
                )
                else super#handle msg
            | Result.Result(Result.Custom(Wrap.SmallSinkRes(snk_, res), _)) ->
                if Sink.equals snk snk_
                then
                (
                    smallresults <- res::smallresults;
                    None
                )
                else super#handle msg
            | _ -> super#handle msg

        method get =
            match results with
            | [] -> None
            | _ ->
            (
                let label = Identifier.pp snk.Sink.name in
                let res = Some(Result.Mixed(label, List.rev results)) in
                results <- [];
                res
            )

        method get_small =
            match smallresults with
            | [] -> None
            | _ ->
            (
                let label = Identifier.pp snk.Sink.name in
                let res = Some(Result.Mixed(label, List.rev smallresults)) in
                smallresults <- [];
                res
            )

        method send =
            match results with
            | [] -> ()
            | _ -> 
            (
                let label = Identifier.pp snk.Sink.name in
                let l = results in
                results <- [];
                Result.send ~verbosity:0 ~prefix:"RESULT" (Result.Mixed(label, List.rev l))
            )

        method send_small =
            match smallresults with
            | [] -> ()
            | _ -> 
            (
                let label = Identifier.pp snk.Sink.name in
                let l = smallresults in
                smallresults <- [];
                Result.send ~verbosity:2 ~prefix:"RESULT" (Result.Mixed(label, List.rev l))
            )
    end

module type Bank =
    sig
        type t

        module Res : Result.CustomResult

        val create : unit -> t

        val add_stats_to_bundle : t -> Stats.Bundle.t -> Stats.Bundle.t

        val source : Source.t -> t -> t
        val sink : Sink.t -> t -> Res.result * bool

        val pp : t -> string
    end

module NoneBank =
    struct
        type t = None

        module ResVal = 
            struct
                type t = None

                let to_generic _ = Result.Str("")
            end

        module Res = Result.Make(ResVal)

        let create () = None
        let add_stats_to_bundle _ b = b
        let source _ _ = None
        let sink _ _ = (Res.create ~label:"None" None), false
        let pp _ = ""
    end

module type State =
    sig
        type t

        val create : ?tag:(string option) -> unit -> t
        val add_stats_to_bundle : Stats.Bundle.t -> t -> Stats.Bundle.t
    end

module NoneState =
    struct
        type t = None

        let create ?(tag = Option.None) () = None

        let add_stats_to_bundle b _ = b
    end

module type Sig =
    sig
        type t
        type bank
        type state

        val create : 
            ?filter_sources:(Source.t -> state -> bool) ->
            ?filter_sinks:(Sink.t -> state -> bool) ->
            ?filter_analyzed_sinks:(Sink.t -> bool -> state -> bool) ->
            ?ignore_sources:bool ->
            ?ignore_sinks:bool ->
            ?monitoring:bool ->
            ?tag:(string option) ->
            string -> t

        val get_name : t -> Identifier.t
        val get_bank : t -> bank
        val set_bank : t -> bank -> t
        val get_state : t -> state
        val set_state : t -> state -> t

        val with_message_handler : (unit -> 'a) -> t -> 'a

        val add_stats_to_bundle : Stats.Bundle.t -> t -> Stats.Bundle.t

        val set_filter_sources : (Source.t -> state -> bool) -> t -> t
        val set_filter_sinks : (Sink.t -> state -> bool) -> t -> t
        val set_filter_analyzed_sinks : (Sink.t -> bool -> state -> bool) -> t -> t
        val default_filter_sources : Source.t -> state -> bool
        val default_filter_sinks : Sink.t -> state -> bool
        val default_filter_analyzed_sinks : Sink.t -> bool -> state -> bool
        val set_ignore_sources : bool -> t -> t
        val set_ignore_sinks : bool -> t -> t
        val set_monitoring : bool -> t -> t

        val set_min_verbosity : int -> t -> t

        module type CallbackQueue =
            sig
                type arg

                val find : string -> t -> (arg -> t -> t)
                val add : ?priority:int -> string -> (arg -> t -> t) -> t -> t
                val add_const : ?priority:int -> string -> (arg -> t -> unit) -> t -> t
                val remove : string -> t -> t
                val fold : (string -> int -> (arg -> t -> t) -> 'a -> 'a) -> t -> 'a -> 'a
                val run : arg -> t -> t
            end

        module TraceCallbacks :
            sig
                include CallbackQueue with type arg = Trace.trace
            end

        module SourceCallbacks :
            sig
                include CallbackQueue with type arg = Source.t
            end

        module SinkCallbacks :
            sig
                include CallbackQueue with type arg = Sink.t
            end

        module InstructionCallbacks : 
            sig
                include CallbackQueue with type arg = Instruction.t

                val bank_pp_callback : arg -> t -> unit
            end

        module FunctionCallbacks :
            sig
                include CallbackQueue with type arg = Function.t

                val on_callret : ?priority:int -> Function.CallId.t -> string -> (Function.func list -> t -> t) -> t -> t
                val on_callret_const : ?priority:int -> Function.CallId.t -> string -> (Function.func list -> t -> unit) -> t -> t
                
                val start_callback :
                    ?condition:(Function.t -> bool) ->
                    ?todo:(t -> t) ->
                    ?name:string ->
                    ?add_sink_checks:bool ->
                    ?add_source_checks:bool ->
                    ?fini_stats:bool ->
                    arg ->
                    t -> t
            end

        module EndCallbacks : 
            sig
                include CallbackQueue with type arg = int

                val pp_stats_callback : arg -> t -> unit
            end

        module SignalCallbacks :
            sig
                include CallbackQueue with type arg = Message.signal list
            end

        val analyze : Trace.trace -> t -> t
        val fini : int -> t -> t
        val options : string -> unit
    end

let cnt = ref 0

module Make (B : Bank) (S : State) =
    struct
        let monit_opt = new Options.TaggedOptions.opt false

        module AnalysisStats =
            struct
                module AddrMap = Map.Make (Z)

                type t =
                    {
                        fun_cnt : Stats.counter;
                        instr_cnt : Stats.counter;
                        unique_instr_cnt : Stats.counter;
                        source_cnt : Stats.counter;
                        sink_cnt : Stats.counter;
                        positive_cnt : Stats.counter;
                        seen_instrs : bool AddrMap.t;
                        analyze_time : Stats.timer;
                    }

                let create () =
                    let fun_cnt = new Stats.counter "fun cnt" "Processed function calls" 0 in
                    let instr_cnt = new Stats.counter "instr cnt" "Processed instructions" 0 in
                    let unique_instr_cnt = new Stats.counter "uniq instr cnt" "Unique processed instructions" 0 in
                    let source_cnt = new Stats.counter "src cnt" "Sources" 0 in
                    let sink_cnt = new Stats.counter "snk cnt" "Sinks" 0 in
                    let positive_cnt = new Stats.counter "positive cnt" "Positive sinks" 0 in

                    let seen_instrs = AddrMap.empty in
                    let analyze_time = new Stats.timer "analyze time" "Analysis runtime" in
                    {fun_cnt; instr_cnt; unique_instr_cnt; source_cnt; sink_cnt; positive_cnt; seen_instrs; analyze_time}

                let check_unique i s =
                    try
                        ignore @@ AddrMap.find i.Instruction.address s.seen_instrs;
                        s
                    with Not_found ->
                    (
                        s.unique_instr_cnt#incr;
                        let seen_instrs = AddrMap.add i.address true s.seen_instrs in
                        {s with seen_instrs}
                    )
            end

        module CallIdMap = Map.Make(Function.CallId)

        type trc_arg = Trace.trace
        type src_arg = Source.t
        type snk_arg = Sink.t
        type ins_arg = Instruction.t
        type fnc_arg = Function.t
        type fnc_callret_arg = Function.func list
        type fin_arg = int
        type sig_arg = Message.signal list

        type 'a callbacks =
            {
                trc : (string * (int * (trc_arg -> 'a -> 'a))) list;
                src : (string * (int * (src_arg -> 'a -> 'a))) list;
                snk : (string * (int * (snk_arg -> 'a -> 'a))) list;
                ins : (string * (int * (ins_arg -> 'a -> 'a))) list;
                fnc : (string * (int * (fnc_arg -> 'a -> 'a))) list;
                fnc_callret : (string * (int * (fnc_callret_arg -> 'a -> 'a))) list CallIdMap.t;
                fin : (string * (int * (fin_arg -> 'a -> 'a))) list;
                sign : (string * (int * (sig_arg -> 'a -> 'a))) list;
            }

        type bank = B.t
        type state = S.t

        module SinkMap = Map.Make (Sink)

        type options =
            {
                filter_sources : Source.t -> state -> bool;
                filter_sinks : Sink.t -> state -> bool;
                filter_analyzed_sinks : Sink.t -> bool -> state -> bool;
                ignore_sources : bool;
                ignore_sinks : bool;
                monitoring : bool
            }

        type t =
            {
                name : Identifier.t;
                bank : B.t;
                state : S.t;
                stats : AnalysisStats.t;
                mh : mh;
                callbacks : t callbacks;
                options : options;
                rerun : bool
            }

        let default_filter_sources _ _ =
            true

        let default_filter_sinks snk _ =
            true

        let default_filter_analyzed_sinks _ positive _ =
            positive

        let create 
            ?(filter_sources = default_filter_sources)
            ?(filter_sinks = default_filter_sinks)
            ?(filter_analyzed_sinks = default_filter_analyzed_sinks)
            ?(ignore_sources = false)
            ?(ignore_sinks = false)
            ?monitoring
            ?(tag = None)
                name =
            let name = Identifier.create ~cnt ~tag name in
            let bank = B.create () in
            let state = S.create ~tag () in
            let stats = AnalysisStats.create () in
            let trc = [] in
            let src = [] in
            let snk = [] in
            let ins = [] in
            let fnc = [] in
            let fnc_callret = CallIdMap.empty in
            let fin = [] in
            let sign = [] in
            let monitoring = monit_opt#maybe_get ?tag monitoring in
            let callbacks = {trc; src; snk; ins; fnc; fnc_callret; fin; sign} in
            let options = {filter_sources; filter_sinks; filter_analyzed_sinks; ignore_sources; ignore_sinks; monitoring} in
            let mh = new mh name in
            {name; bank; state; stats; mh; callbacks; options; rerun = false}
            
        let get_name a =
            a.name

        let get_bank a =
            a.bank

        let set_bank a bank =
            {a with bank}

        let get_state a =
            a.state

        let set_state a state = 
            {a with state}

        let with_message_handler f a =
            Message.with_handler f (a.mh :> Message.handler)

        let add_stats_to_bundle b a =
            b
                |> B.add_stats_to_bundle a.bank
                |> Stats.Bundle.add_stat (a.stats.fun_cnt :> Stats.stat)
                |> Stats.Bundle.add_stat (a.stats.instr_cnt :> Stats.stat)
                |> Stats.Bundle.add_stat (a.stats.unique_instr_cnt :> Stats.stat)
                |> Stats.Bundle.add_stat (a.stats.source_cnt :> Stats.stat)
                |> Stats.Bundle.add_stat (a.stats.sink_cnt :> Stats.stat)
                |> Stats.Bundle.add_stat (a.stats.positive_cnt :> Stats.stat)
                |> Stats.Bundle.add_stat (a.stats.analyze_time :> Stats.stat)

        let set_filter_sources filter_sources a =
            {a with options = {a.options with filter_sources}}

        let set_filter_sinks filter_sinks a =
            {a with options = {a.options with filter_sinks}}

        let set_filter_analyzed_sinks filter_analyzed_sinks a =
            {a with options = {a.options with filter_analyzed_sinks}}

        let set_ignore_sources ignore_sources a =
            {a with options = {a.options with ignore_sources}}

        let set_ignore_sinks ignore_sinks a =
            {a with options = {a.options with ignore_sinks}}

        let set_monitoring monitoring a =
            {a with options = {a.options with monitoring}}

        let check_sos snk a =
            if (not @@ Trace.terminated ()) && Str.string_match !sos (Identifier.pp snk.Sink.name) 0
            then 
            (
                Message.Wrap.send (Message.Wrap.BigWarning(lazy (Printf.sprintf "terminating after sink <%s>" @@ Identifier.pp snk.Sink.name)));
                Trace.terminate_next ()
            )

        let set_min_verbosity v a =
            a.mh#set_min_verb v;
            a

        module type CallbackList =
            sig
                type arg

                val get : t -> (string * (int * (arg -> t -> t))) list
                val set : t -> (string * (int * (arg -> t -> t))) list -> t
            end

        module type CallbackQueue =
            sig
                type arg

                val find : string -> t -> (arg -> t -> t)
                val add : ?priority:int -> string -> (arg -> t -> t) -> t -> t
                val add_const : ?priority:int -> string -> (arg -> t -> unit) -> t -> t
                val remove : string -> t -> t
                val fold : (string -> int -> (arg -> t -> t) -> 'a -> 'a) -> t -> 'a -> 'a
                val run : arg -> t -> t
            end

        module CallbackQueueMaker (CL : CallbackList) = 
            struct
                let find key a =
                    match List.assoc key @@ CL.get a with (_, cb) -> cb

                let add ?(priority = 0) key (e : (CL.arg -> t -> t)) a =
                    try
                        ignore @@ List.find (fun (k, _) -> k = key) @@ CL.get a;
                        raise (Failure (Printf.sprintf "CallbackQueue.add: key %s already in use" key))
                    with Not_found ->
                    (
                        let node = (key, (priority, e)) in
                        let rec aux l =
                            match l with
                            | h::t -> 
                            (
                                let n = match h with (_, (n, _)) -> n in
                                if priority > n
                                then node::l
                                else h::(aux t)
                            )
                            | [] -> [node]
                        in
                        CL.set a @@ aux @@ CL.get a
                    )

                let add_const ?(priority = 0) key (e : (CL.arg -> t -> unit)) a =
                    add ~priority key (fun arg a -> e arg a; a) a

                let remove key a =
                    CL.set a @@ List.remove_assoc key @@ CL.get a

                let fold f a b =
                    List.fold_left (fun res e -> match e with (key, (prio, e)) -> f key prio e res) b @@ CL.get a

                let run arg a =
                    a.stats.analyze_time#start;
                    let a = fold 
                        (
                            fun key prio e a -> 
                                Message.Wrap.send (Message.Wrap.Debug("CALLBACK", lazy(Printf.sprintf "%s <%d>" key prio)));
                                e arg a
                        )
                        a a
                    in
                    a.stats.analyze_time#stop;
                    a
            end

        module TraceCallbacks =
            struct
                module TraceCL =
                    struct
                        type arg = trc_arg

                        let get a =
                            a.callbacks.trc

                        let set a trc =
                            let callbacks = {a.callbacks with trc} in
                            {a with callbacks}
                    end

                type arg = trc_arg

                include CallbackQueueMaker (TraceCL)
            end

        module SourceCallbacks =
            struct
                module SourceCL =
                    struct
                        type arg = src_arg

                        let get a =
                            a.callbacks.src

                        let set a src =
                            let callbacks = {a.callbacks with src} in
                            {a with callbacks}
                    end

                type arg = src_arg

                include CallbackQueueMaker (SourceCL)

                let run src a =
                    if (not a.options.ignore_sources) 
                        && Str.string_match !selected_sources (Identifier.pp src.Source.name) 0
                        && a.options.filter_sources src a.state
                    then 
                    (
                        a.stats.source_cnt#incr;
                        Message.Wrap.send (Wrap.Source(src));
                        run src {a with bank = B.source src a.bank}
                    )
                    else a
            end

        module SinkCallbacks =
            struct
                module SinkCL =
                    struct
                        type arg = snk_arg

                        let get a =
                            a.callbacks.snk

                        let set a snk =
                            let callbacks = {a.callbacks with snk} in
                            {a with callbacks}
                    end

                type arg = snk_arg

                include CallbackQueueMaker (SinkCL)

                let run snk a =
                    if (not a.options.ignore_sinks) 
                        && Str.string_match !selected_sinks (Identifier.pp snk.Sink.name) 0
                        && a.options.filter_sinks snk a.state
                    then
                    (
                        Message.Wrap.send (Wrap.Sink(snk));
                        a.stats.sink_cnt#incr;
                        let mh = new sinkres_catcher snk in
                        let do_run () =
                            let snkres = Result.mk_sto_expr ~label:"Sink" snk.Sink.expr in
                            let res, positive = B.sink snk a.bank in
                            if a.options.filter_analyzed_sinks snk positive a.state
                            then 
                            (
                                if positive then a.stats.positive_cnt#incr;
                                Message.Wrap.send (Wrap.SinkResult(snk, Result.Generic(snkres)));
                                Message.Wrap.send (Wrap.SinkResult(snk, B.Res.wrap res));
                                if !sops then Trace.terminate_next ();
                                run snk a
                            )
                            else
                            (
                                Message.Wrap.send (Wrap.SmallSinkResult(snk, Result.Generic(snkres)));
                                Message.Wrap.send (Wrap.SmallSinkResult(snk, B.Res.wrap res));
                                a
                            )
                        in
                        let a = Message.with_handler do_run (mh :> Message.handler) in
                        mh#send;
                        mh#send_small;
                        a
                    )
                    else a
            end

        module InstructionCallbacks =
            struct
                module InstructionCL =
                    struct
                        type arg = ins_arg

                        let get a =
                            a.callbacks.ins

                        let set a ins =
                            let callbacks = {a.callbacks with ins} in
                            {a with callbacks}
                    end

                type arg = ins_arg

                include CallbackQueueMaker (InstructionCL)

                let run instr a =
                    a.stats.instr_cnt#incr;
                    Message.Wrap.send (Wrap.Instruction(instr));
                    let stats = AnalysisStats.check_unique instr a.stats in
                    run instr {a with stats}

                (*generic callbacks*)

                let bank_pp_callback instr a =
                    Message.Wrap.send (Message.Wrap.SmallDebug("BANK", lazy (B.pp a.bank)))
            end

        module EndCallbacks =
            struct
                module EndCL =
                    struct
                        type arg = fin_arg

                        let get a =
                            a.callbacks.fin

                        let set a fin =
                            let callbacks = {a.callbacks with fin} in
                            {a with callbacks}
                    end

                type arg = fin_arg

                include CallbackQueueMaker (EndCL)

                let pp_stats_callback _ a =
                    let b = S.add_stats_to_bundle (add_stats_to_bundle (Stats.Bundle.create @@ Identifier.pp a.name) a) @@ get_state a in
                    Message.Wrap.send (Stats.Wrap.BigStats(b))
            end

        module SignalCallbacks =
            struct
                module SigCL =
                    struct
                        type arg = sig_arg

                        let get a =
                            a.callbacks.sign

                        let set a sign =
                            let callbacks = {a.callbacks with sign} in
                            {a with callbacks}
                    end

                    type arg = sig_arg

                    include CallbackQueueMaker (SigCL)
            end

        module FunctionCallbacks =
            struct
                module FunctionCL =
                    struct
                        type arg = fnc_arg

                        let get a =
                            a.callbacks.fnc

                        let set a fnc =
                            let callbacks = {a.callbacks with fnc} in
                            {a with callbacks}
                    end

                type arg = fnc_arg

                include CallbackQueueMaker (FunctionCL)

                module CallRetCallbacks =
                    struct
                        module CallRetCL =
                            struct
                                type arg = fnc_callret_arg

                                let noneselected = Function.CallId.create (-1) (-1)
                                let selected = ref noneselected

                                let get a =
                                    try
                                        CallIdMap.find !selected a.callbacks.fnc_callret
                                    with Not_found -> []

                                let set a l =
                                    let fnc_callret = CallIdMap.add !selected l a.callbacks.fnc_callret in
                                    let callbacks = {a.callbacks with fnc_callret} in
                                    {a with callbacks}
                            end

                        type arg = fnc_callret_arg

                        include CallbackQueueMaker (CallRetCL)
                        
                        let drop call_id a =
                            let fnc_callret = CallIdMap.remove call_id a.callbacks.fnc_callret in
                            let callbacks = {a.callbacks with fnc_callret} in
                            {a with callbacks}

                        let drop_all_above call_id a =
                            let pivot = Function.CallId.create 0 (call_id.Function.CallId.stack_id - 1) in
                            let fnc_callret, _, _ = CallIdMap.split pivot a.callbacks.fnc_callret in
                            let callbacks = {a.callbacks with fnc_callret} in
                            {a with callbacks}
                    end

                let on_callret ?(priority = 0) call_id name cb a =
                    CallRetCallbacks.CallRetCL.selected := call_id;
                    let a = CallRetCallbacks.add ~priority name cb a in
                    CallRetCallbacks.CallRetCL.selected := CallRetCallbacks.CallRetCL.noneselected;
                    a

                let on_callret_const ?(priority = 0) call_id name cb a =
                    CallRetCallbacks.CallRetCL.selected := call_id;
                    let a = CallRetCallbacks.add_const ~priority name cb a in
                    CallRetCallbacks.CallRetCL.selected := CallRetCallbacks.CallRetCL.noneselected;
                    a

                let run func a =
                    (
                        match func with
                        | Function.Entry(_) -> a.stats.fun_cnt#incr
                        | _ -> ()
                    )
                    ;
                    Message.Wrap.send (Wrap.Function(func));
                    let a = run func a in
                    match func with
                    | Function.CallRet(call_id, funcs) ->
                    (
                        CallRetCallbacks.CallRetCL.selected := call_id;
                        let a = CallRetCallbacks.run funcs a in
                        CallRetCallbacks.CallRetCL.selected := CallRetCallbacks.CallRetCL.noneselected;
                        CallRetCallbacks.drop call_id @@ CallRetCallbacks.drop_all_above call_id a
                    )
                    | _ -> a

                (*generic callbacks*)

                let cond_main func =
                    let main = Trace.get_main_function () in
                    match func with
                    | Function.Entry(func) -> main = "" || func.Function.fname = main
                    | _ -> false

                let start_callback 
                        ?(condition = cond_main) 
                        ?(todo = fun a -> a) 
                        ?(name = "start") 
                        ?(add_sink_checks = true)
                        ?(add_source_checks = true)
                        ?(fini_stats = true)
                        func a =
                    let cond c f a =
                        if c then f a else a
                    in
                    if condition func
                    then
                    (
                        a
                            |> remove name
                            |> InstructionCallbacks.add_const ~priority:(-1) "pp bank" InstructionCallbacks.bank_pp_callback
                            |> cond fini_stats
                                (EndCallbacks.add_const ~priority:100 "fini stats" EndCallbacks.pp_stats_callback)
                            |> todo
                            |> fun a -> {a with rerun = true}
                    )
                    else a
            end

        let rec analyze trace a =
            let maybe_monit kind f =
                if a.options.monitoring 
                then
                (
                    let a = Monitoring.with_monit kind f in
                    match a.mh#get_signals with
                    | [] -> a
                    | l -> SignalCallbacks.run l a
                )
                else f ()
            in
            let do_analyze () =
                let a = maybe_monit (Monitoring.Trc(trace)) (fun () -> TraceCallbacks.run trace a) in
                match trace with
                | Ins(instr) -> maybe_monit (Monitoring.Ins(instr)) (fun () -> InstructionCallbacks.run instr a)
                | Fun(Entry(func) as f) -> 
                (
                    if Str.string_match !saf func.Function.fname 0 
                    then 
                    (
                        Trace.terminate_next ();
                        Message.Wrap.send (Message.Wrap.BigWarning(lazy (Printf.sprintf "stopping on entering %s" func.Function.fname)))
                    )
                    ;
                    maybe_monit (Monitoring.Func(f)) (fun () -> FunctionCallbacks.run f a)
                )
                | Fun(func) -> maybe_monit (Monitoring.Func(func)) (fun () -> FunctionCallbacks.run func a)
                | Src(src) -> maybe_monit (Monitoring.Src(src)) (fun () -> SourceCallbacks.run src a)
                | Snk(snk) -> 
                (
                    check_sos snk a;
                    maybe_monit (Monitoring.Snk(snk)) (fun () -> SinkCallbacks.run snk a)
                )
                | _ -> a 
            in
            let a = Message.with_handler do_analyze (a.mh :> Message.handler) in
            if a.rerun
            then analyze trace {a with rerun = false}
            else a

        let fini signal a =
            let fini () =
                EndCallbacks.run signal a
            in
            let maybe_monit () =
                if a.options.monitoring 
                then
                (
                    let a = Monitoring.with_monit Monitoring.Fini fini in
                    match a.mh#get_signals with
                    | [] -> a
                    | l -> SignalCallbacks.run l a
                )
                else fini ()
            in
            Message.with_handler maybe_monit (a.mh :> Message.handler)

        let options name =
            Options.register_policy_option name "-monitor" (Options.TaggedOptions.Bool(monit_opt)) "Enable monitoring."
    end

let _ =
    Options.register_option ("-select-sinks", Arg.String(fun s -> selected_sinks := Utils.pattern_list @@ String.split_on_char ';' s), "Explicitly select sinks (format: name;<regex>;...).");
    Options.register_alias "-select-sinks" "-ssinks";
    Options.register_option ("-select-sources", Arg.String(fun s -> selected_sources := Utils.pattern_list @@ String.split_on_char ';' s), "Explicitly select sources (format: name;<regex>;...).");
    Options.register_alias "-select-sources" "-ssources";
    Options.register_option ("-stop-on-positive-sink", Arg.Set sops, "Stop analysis on positive sink.");
    Options.register_alias "-stop-on-positive-sink" "-sops";
    Options.register_option ("-stop-on-sink", Arg.String(fun s -> sos := Utils.pattern_list @@ String.split_on_char ';' s), "Stop analysis when a matching sink is found (format: name;<regex>;...).");
    Options.register_alias "-stop-on-sink" "-sos";
    Options.register_option ("-stop-at-function", Arg.String(fun s -> saf := Utils.pattern_list @@ String.split_on_char ';' s), "Stop analysis when entering a specified function (format: name;<regex>;...).");
    Options.register_alias "-stop-at-function" "-saf"

module Utils =
    struct
        let strlen addr =
            let rec aux size =
                let sto = Storage.Memory(Z.add addr size, 1) in
                let byte = Trace.stoval sto in
                if byte = Z.zero
                then size
                else aux @@ Z.add size Z.one
            in
            aux Z.zero

        let get_ret_addr () =
            let rsp = Trace.stoval (Storage.Register("rsp", 8)) in
            Trace.stoval (Storage.Memory(rsp, 8))
    end
