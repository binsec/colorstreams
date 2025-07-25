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

let opts = ref "--delta 0.05"

let mk_cmd ~opts ~projections file =
    let projs, cnfs = PMCount.Popcon.get_cnf ~projections file in
    let projline = (List.fold_left (fun res e -> res ^ " " ^ e) "c ind" projs) ^ " 0" in
    let cnf = File.create ~keep:true ((File.get_name file) ^ "_ganak") ".cnf" in
    File.write_lines (projline::cnfs) cnf;
    if !PMCount.Solver.timeout > 0
    then Printf.sprintf "timeout -v %f ganak %s %s" ((Float.of_int !PMCount.Solver.timeout) /. 1000.) opts @@ File.get_path cnf
    else Printf.sprintf "ganak %s %s" opts @@ File.get_path cnf

let parse_out (out, err, (r : PMCount.Count.result)) =
    let resexp = Str.regexp {|s \(exact\|approx\) arb int \([0-9]+\)|} in
    let timeoutexp = Str.regexp {|timeout: sending signal|} in
    let rec check_out l =
        match l with
        | line::t -> 
        (
            try
                ignore @@ Str.search_forward resexp line 0;
                let count = Str.matched_group 2 line |> Z.of_string in
                {r with result = PMCount.CountVal.Count(count)}
            with Not_found -> 
            (
                try
                    ignore @@ Str.search_forward timeoutexp line 0;
                    Message.Wrap.send (Message.Wrap.BigWarning(lazy ("ganak: timeout")));
                    {r with result = PMCount.CountVal.Timeout}
                with Not_found -> check_out t
            )
        )
        | _ -> {r with result = PMCount.CountVal.Error}
    in
    let res = check_out out in
    match res.result with
    | Error -> check_out err
    | _ -> res

let _ = 
    PMCount.Solver.register "ganak" "Probabilistic exact model counter." (mk_cmd ~opts:!opts) parse_out;
    Options.register_option ("-ganak-opts", Arg.Set_string(opts), Printf.sprintf "Set command line options for ganak (default: %s)." !opts)
