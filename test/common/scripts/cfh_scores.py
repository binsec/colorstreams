import matplotlib.pyplot as plt
import tabulate as tab
import results as res
import sys
import numpy as np
import domains
import collections

short = {"out-of-bounds": "OOB", "use-after-free": "UAF", "memory mapping violation": "MMV", "read": "R", "write": "W"}

def get_scores_cfh(data):
    scores = []
    for prog, d in data.items():
        d = d["results"]
        for k in (k for k in d.keys() if "CFH Capa" in k):
            for detec in (k for k in d[k]["result"].keys() if "Detection " in k):
                num = int(detec.split(' ')[1])
                results = d[k]["result"][detec]["result"]
                typ = results[detec]["result"]["kind"]["result"]
                reports = results["report"]["result"]
                try:
                    score = reports["overall score"]["result"]["max"]["result"]
                except Exception as _:
                    score = 0.0
                entry = {}
                entry["Bug"] = prog
                entry["ID"] = num
                entry["Type"] = typ
                entry["QC Score"] = int(results["ptr"]["result"]["S&S"]["result"]["Count"]["result"]["max"]["result"][2:]) / 2**64
                entry["wQC Score"] = score
                scores.append(entry)
    def sort_key(entry):
        return entry["wQC Score"]
    scores.sort(key = sort_key)
    scores.reverse()
    return scores

def mk_table():
    table = []
    headers = ["Bug", "ID", "Type", "QC Score", "wQC Score"]
    for entry in get_scores_cfh(res.load()):
        def fmtfloat(k):
            entry[k] = format(entry[k], ".3g")
        fmtfloat("QC Score")
        fmtfloat("wQC Score")
        entry = [entry[k] for k in headers]
        table.append(entry)
    table = tab.tabulate(table, headers = headers, floatfmt = ".3g", tablefmt = res.fmt)
    print("CFH capability scores:")
    print()
    print(table)

if __name__ == "__main__":
    res.help_msg("\nDisplay CFH capability scores for vulnerabilities.")
    res.options_done()
    mk_table()
