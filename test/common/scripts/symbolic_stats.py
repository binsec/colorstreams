import tabulate as tab
import results as res
import numpy as np
import sys

headers = ["Bug", "Analysis", "Symbolic Instr.", "Total Instr.", "SE Runtime", "Tracing Runtime", "Total Runtime"]

def get_stats(data, all_runtimes = False):
    stats = []
    for prog, d in data.items():
        d = d["stats"]
        k = [k for k in d.keys() if k.startswith("Symbolic")]
        for k in k:
            entry = {}
            entry["Bug"] = prog
            entry["Analysis"] = k
            entry["Symbolic Instr."] = d[k]["symb_cnt"]
            entry["Total Instr."] = d[k]["instr cnt"]
            #entry["Concrete"] = entry["Total"] - entry["Symbolic"]
            entry["SE Runtime"] = d[k]["se_runtime"]
            entry["Tracing Runtime"] = d["Tracing"]["runtime"]
            entry["Total Runtime"] = d["General"]["runtime"]
            stats.append(entry)
        if all_runtimes and k == []:
            entry = {}
            entry["Bug"] = prog
            entry["Analysis"] = "-"
            entry["Symbolic Instr."] = "-"
            entry["Total Instr."] = "-"
            entry["SE Runtime"] = 0.0
            entry["Tracing Runtime"] = d["Tracing"]["runtime"]
            entry["Total Runtime"] = d["General"]["runtime"]
            stats.append(entry)
    stats.sort(key = lambda e: e["Bug"])
    return stats

if __name__ == "__main__":
    res.help_msg("\n[SPECIFIC OPTIONS]")
    all_runtimes = res.opt_enable("-all", "Also include runtimes from runs without symbolic execution")
    res.help_msg("\nDisplay SE-related statistics.")
    res.options_done()
    table = []
    for entry in get_stats(res.load(), all_runtimes):
        entry = [entry[k] for k in headers]
        table.append(entry)
    print("Executed instructions and analysis runtime (s) for each bug:")
    print()
    print(tab.tabulate(table, headers = headers, tablefmt = res.fmt))
    print()
    print("Total SE runtime: ", sum([e[-3] for e in table]))
    print("Total Tracing runtime: ", sum([e[-2] for e in table]))
    print("Total overall runtime: ", sum([e[-1] for e in table]))
