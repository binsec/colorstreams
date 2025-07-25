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

let parse_config_file path =
    let file = File.from_file path in
    let lines = File.read file in
    let nocomments = Str.regexp {|^[ ]*\([^#]*[^# ]+\)[ ]*|} in
    let exp = Str.regexp {|^\([^= ]+\)\([ ]*=[ ]*\(.*\)\)?$|} in
    let opts = List.fold_right
        (
            fun line res ->
                if Str.string_match nocomments line 0
                then
                (
                    let line = Str.matched_group 1 line in
                    if Str.string_match exp line 0
                    then 
                    (
                        let opt = Str.matched_group 1 line in
                        try
                            let value = 
                                let value = Str.matched_group 3 line in
                                if String.starts_with ~prefix:"\"" value && String.ends_with ~suffix:"\"" value
                                then String.sub value 1 ((String.length value) - 2)
                                else value
                            in
                            if opt = "target"
                            then res @ [value]
                            else ("-" ^ opt)::value::res
                        with Not_found -> ("-" ^ opt)::res
                    )
                    else raise (Failure (Printf.sprintf "invalid config line <%s> in %s" line path))
                )
                else res
        )
        lines []
    in
    Message.Wrap.send (Message.Wrap.SmallInfo(lazy (Printf.sprintf "Options from config file <%s>: %s" path @@ Utils.pp_list ~sep:" " (fun e -> e) opts)));
    Options.parse_args_argv (Array.of_list ("colorstreams"::opts))

let _ =
    Options.register_option ("-config", Arg.String(fun s -> List.iter parse_config_file @@ String.split_on_char ';' s), "Specify a configuration file (format: file1;file2;...).")
