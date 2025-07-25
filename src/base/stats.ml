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

open Yojson.Safe

let stat_freq = ref 0

class virtual stat ?(visible = true) (name : string) =
    object (self)
        method name = name
        method visible = visible
        method virtual pp : string
        method virtual to_json : Yojson.Safe.t
    end

class sstat ?(visible = true) name desc =
    object(self)
        inherit stat ~visible name

        val mutable s = None

        method set s_ =
            s <- Some(s_)

        method get =
            match s with
            | Some(s) -> s
            | None -> "N/A"

        method pp =
            desc ^ ": " ^ self#get

        method to_json =
            match s with
            | Some(s) -> `String(s)
            | None -> `Null
    end

class flag ?(visible = true) name desc =
    object(self)
        inherit stat ~visible name

        val mutable flag = false

        method get =
            flag

        method set =
            flag <- true

        method unset =
            flag <- false

        method pp =
            if flag
            then desc ^ ": true"
            else desc ^ ": false"

        method to_json =
            `Bool(flag)
    end

class counter ?(visible = true) ?(callback = fun _ -> ()) name desc init =
    object (self)
        inherit stat ~visible name

        val mutable cnt = init

        method get =
            cnt

        method set i =
            cnt <- i

        method incr =
            if cnt = max_int
            then Message.Wrap.send (Message.Wrap.Warning(lazy (Printf.sprintf "%s: counter overflow" name)))
            else cnt <- cnt + 1
            ;
            callback cnt

        method decr =
            if cnt = 0
            then Message.Wrap.send (Message.Wrap.Warning(lazy (Printf.sprintf "%s: cannot decrement null counter" name)))
            else cnt <- cnt - 1

        method pp =
            Printf.sprintf "%s: %d" desc cnt

        method to_json =
            `Int(cnt)
    end

class timer ?(visible = true) name desc =
    object (self)
        inherit stat name

        val mutable total = Float.zero
        val mutable start = Float.zero
        val mutable running = false

        method running =
            running

        method start =
            if not running
            then
            (
                running <- true;
                start <- Unix.gettimeofday ()
            )

        method stop =
            if running
            then
            (
                running <- false;
                let stop = Unix.gettimeofday () in
                total <- total +. stop -. start
            )

        method get =
            if running
            then
            (
                self#stop;
                self#start
            )
            ;
            total

        method reset =
            self#stop;
            total <- Float.zero

        method pp =
            Printf.sprintf "%s: %0.3fs" desc self#get

        method to_json =
            `Float(self#get)
    end

module Bundle =
    struct
        type t =
            {
                name : string;
                report : bool;
                stats : (string * stat) list
            }

        let create ?(report = true) name =
            {name; report; stats = []}

        let get_name t =
            t.name

        let add_stat s b =
            let stats = (s#name, s)::b.stats in
            {b with stats}

        let get_stat name b =
            List.assoc name b.stats

        let pp_stats b =
            List.fold_left (fun res (_, e) -> if e#visible then (e#pp)::res else res) [] b.stats

        let pp b =
            let lst = List.rev @@ pp_stats b in
            b.name ^ " stats:\n" ^ (List.fold_left (fun res e -> "├─" ^ e ^ "\n" ^ res) ("└─" ^ (List.hd lst))) (List.tl lst)
            
        let to_json b =
            `Assoc(List.fold_left (fun res (name, stat) -> if stat#visible then (name, stat#to_json)::res else res) [] b.stats)
    end

module Wrap =
    struct
        type Message.ext_value += ExtStats of Bundle.t
        type Message.Wrap.wrap += 
            | Stats of Bundle.t
            | BigStats of Bundle.t

        let unwrapper next = function
            | Stats(b) -> 2, false, "STATS", (ExtStats(b)), lazy (Bundle.pp b)
            | BigStats(b) -> 1, false, "STATS", (ExtStats(b)), lazy (Bundle.pp b)
            | w -> next w

        let _ = Message.Wrap.register_unwrapper unwrapper
    end

module StringMap = Map.Make (String)

let bundles = ref StringMap.empty
let order = ref []

let register ?(big = true) ?(silent = false) b =
    try
        ignore @@ StringMap.find b.Bundle.name !bundles
    with Not_found -> 
    (
        bundles := StringMap.add b.name (b, big, silent) !bundles;
        order := b.name::!order
    )

let file = File.create ~temporary:false "stats" ".log"

let log_live () =
    let lines = List.filter_map
        (
            fun e -> 
                try
                    let e, _, _ = StringMap.find e !bundles in
                    if e.report 
                    then Some(Bundle.pp e)
                    else None
                with Not_found -> None
        ) 
        !order
    in
    File.write_lines lines file

let log () =
    List.iter 
        (
            fun e -> 
            (
                try
                    let e, big, silent = StringMap.find e !bundles in
                    if e.report && not silent
                    then 
                    (
                        if big
                        then Message.Wrap.send (Wrap.BigStats(e))
                        else Message.Wrap.send (Wrap.Stats(e))
                    )
                with Not_found -> ()
            )
        ) 
        !order
    ;
    if !stat_freq > 0
    then log_live ()

let to_json () =
    `Assoc
    (
        List.map 
            (
                fun b -> 
                (
                    try
                        let b, _, _ = StringMap.find b !bundles in
                        (Bundle.get_name b), (Bundle.to_json b)
                    with Not_found -> b, (`Null)
                )
            ) 
            !order
    )

module GeneralStats =
    struct
        let live_cb c =
            if !stat_freq > 0 && (c mod !stat_freq) = 0
            then log_live ()

        let runtime = new timer "runtime" "Total runtime"
        let warn_cnt = new counter "warn cnt" "Warnings" 0
        let cnt = new counter ~visible:false ~callback:live_cb "cnt" "Processed lines" 0

        let b = Bundle.create "General"
            |> Bundle.add_stat (cnt :> stat)
            |> Bundle.add_stat (warn_cnt :> stat)
            |> Bundle.add_stat (runtime :> stat)

        let _ = 
            register b
    end

let _ =
    Options.register_option ("-live-stats", Arg.Set_int stat_freq, "Frequency (in terms of processed instructions) at which stats should be written to stats.log.");
    Options.register_option ("-ls", Arg.Set_int stat_freq, "Alias for -live-stats.")
