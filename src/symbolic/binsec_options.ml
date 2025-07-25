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

let init () =
    ignore @@ Binsec.Cli.parse_configuration_file ~filename:"/dev/null";
    Binsec.Formula_options.Theory.set "ABV";
    if !Message.verbosity < 4 
    then 
    (
        Binsec.Formula_options.Logger.quiet ();
        Binsec.Disasm_options.Logger.quiet ();
        Amd64decoder.Amd64ToDba.Logger.quiet ()
    )

let init_from_target () =
    Binsec.Kernel_functions.Loader.set_arch_from_file ~filename:(Options.get_target ())

let _ =
    Options.register_option ("-solver-timeout", Arg.Int Binsec.Formula_options.Solver.Timeout.set, "Timeout for smt solver (seconds).");
    Options.register_alias "-solver-timeout" "-st"
