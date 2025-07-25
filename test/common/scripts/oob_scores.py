import matplotlib.pyplot as plt
import tabulate as tab
import results as res
import sys
import numpy as np
import domains
import collections
import os
import subprocess

short = {"out-of-bounds": "OOB", "use-after-free": "UAF", "memory mapping violation": "MMV", "read": "R", "write": "W"}

def get_scores_oob(data, merged = False):
    scores = []
    hist = []
    for prog, d in data.items():
        d = d["results"]
        for k in (k for k in d.keys() if "OOB Capa" in k):
            for detec in (k for k in d[k]["result"].keys() if "Detection " in k):
                num = int(detec.split(' ')[1])
                results = d[k]["result"][detec]["result"]
                typ = short[results[detec]["result"]["reason"]["result"]["type"]["result"]] + " " + short[results[detec]["result"]["r/w"]["result"]]
                try:
                    if "history" in results["report"]["result"].keys():
                        history = results["report"]["result"]["history"]["result"]
                        for file, d_ in history.items():
                            for i in d_["result"]:
                                hist.append((os.path.basename(file), int(i[2:])))
                except Exception as _:
                    pass
                def get_score(mapping):
                    reports = results["report"]["result"]
                    try:
                        if merged and "full" in reports.keys():
                            reports = reports["full"]["result"]
                        elif "alone" in reports.keys():
                            reports = reports["alone"]["result"]
                        report = reports[mapping]["result"]
                        if "oob values only" in report.keys():
                            report = report["oob values only"]["result"]
                        base_score = report["base score"]["result"]["max"]["result"]
                        size_score = report["size score"]["result"]["max"]["result"]
                        if report["data score"]["result"] == "N/A":
                            data_score = 0.0
                        else:
                            data_score = report["data score"]["result"]["max"]["result"]
                        overall_score = report["overall score"]["result"]["max"]["result"]
                        return base_score, size_score, data_score, overall_score
                    except Exception as _:
                        return 0.0, 0.0, 0.0, 0.0
                stackbase, stacksize, stackdata, stacktot = get_score("[stack]")
                heapbase, heapsize, heapdata, heaptot = get_score("[heap]")
                entry = {}
                entry["Bug"] = prog
                entry["ID"] = num
                entry["Type"] = typ
                if stacktot > 0.0 and stacktot > heaptot:
                    entry["Mapping"] = "stack"
                    entry["Base Score"] = stackbase
                    entry["Size Score"] = stacksize
                    entry["Data Score"] = stackdata
                    entry["Overall Score"] = stacktot
                elif heaptot > 0.0:
                    entry["Mapping"] = "heap"
                    entry["Base Score"] = heapbase
                    entry["Size Score"] = heapsize
                    entry["Data Score"] = heapdata
                    entry["Overall Score"] = heaptot
                else:
                    entry["Mapping"] = "other"
                    entry["Base Score"] = 0.0
                    entry["Size Score"] = 0.0
                    entry["Data Score"] = 0.0
                    entry["Overall Score"] = 0.0
                scores.append(entry)
    if merged:
        scores = [entry for entry in scores if not (entry["Bug"] + ".json", entry["ID"]) in hist]
    def sort_key(entry):
        if entry["Type"].endswith("W"):
            return entry["Overall Score"] + 10
        else: 
            return entry["Overall Score"]
    scores.sort(key = sort_key)
    scores.reverse()
    return scores

def mk_table(merged = False):
    table = []
    headers = ["Bug", "ID", "Type", "Mapping", "Base Score", "Size Score", "Data Score", "Overall Score"]
    for entry in get_scores_oob(res.load(), merged):
        def fmtfloat(k):
            entry[k] = format(entry[k], ".3g")
        fmtfloat("Base Score")
        fmtfloat("Size Score")
        fmtfloat("Overall Score")
        entry = [entry[k] for k in headers]
        table.append(entry)
    table = tab.tabulate(table, headers = headers, floatfmt = ".3g", tablefmt = res.fmt)
    print("OOB capability scores:")
    print()
    print(table)

def mk_bars(merged = False, simpl = None):
    data = get_scores_oob(res.load(), merged)
    fig, ax = plt.subplots(dpi=res.dpi, ncols=2)
    def mk_sub(idx, cond, title):
        ndata = [e for e in data if cond(e)]
        ndata.reverse()
        x = np.arange(len(ndata))
        if simpl:
            bugs = [subprocess.getoutput("echo %s | %s" % (e["Bug"], simpl)) + "#%d" % e["ID"] for e in ndata]
        else:
            bugs = [e["Bug"] + "#%d" % e["ID"] for e in ndata]
        def mk_frac(e, s): 
            if e[s] == 0:
                return 0
            else: 
                return e["Overall Score"] * e[s] / (e["Base Score"] + e["Size Score"] + e["Data Score"])
        basescores = [mk_frac(e, "Base Score") for e in ndata]
        sizescores = [mk_frac(e, "Size Score") for e in ndata]
        datascores = [mk_frac(e,"Data Score") for e in ndata]
        ax[idx].barh(x, basescores, label = "Base", color = "lightgreen")
        ax[idx].barh(x, sizescores, left = basescores, label = "Size", color = "lightblue")
        if any(score > 0 for score in datascores):
            ax[idx].barh(x, datascores, left = [b + s for b, s in zip(basescores, sizescores)], label = "Data", color = "pink")
        ax[idx].set_yticks(x, bugs, ha = "right")
        ax[idx].set_xlim(0, int(max([a + b + c for (a, b, c) in zip(basescores, sizescores, datascores)]) * 10 + 1) / 10)
        ax[idx].set_title(title)
        if len(basescores) > 0:
            ax[idx].legend(loc = res.legendloc)
    mk_sub(0, lambda e: e["Type"].endswith("W"), "Memory Write Bugs")
    mk_sub(1, lambda e: e["Type"].endswith("R"), "Memory Read Bugs")
    if res.title:
        fig.suptitle("OOB Capability Scores")
    fig.supxlabel("Score")
    fig.tight_layout()
    res.show_or_save()

def weight_inv(lo, hi):
    return abs(res.log2(hi + 1) - res.log2(lo))

def weight_squareinv(lo, hi):
    return abs((1 / (lo + 1)) - (1 / (hi + 2)))

def weight_sqrtinv(lo, hi):
    def sqrt(n):
        if n >= 2**64:
            return 2**(res.log2(n) / 2)
        else:
            return np.sqrt(n)
    return abs(2 * sqrt(hi) - 2 * sqrt(lo))

def get_basic_scores(data, splits = res.sns_splits, sink_filter = res.sfilter, w = weight_inv):
    scores = collections.OrderedDict()
    def maybe_make_entry(entry):
        name = domains.mk_name(entry)
        if not name in scores.keys():
            scores[name] = {"Bug": entry["Bug"], "Sink": entry["Sink"], "Size": entry["Size"], "QC": None, "wQC": None}
        return name
    def get_sns(sns):
        for _, entries in domains.get_sns_from_symb(data, name = sns, splits = splits, sink_filter = sink_filter).items():
            for entry in entries:
                name = maybe_make_entry(entry)
                if entry["Domain"]:
                    score = 0
                    for itv in entry["Domain"]:
                        score += w(itv["lo"], itv["hi"]) * itv["card_hi"] / (itv["hi"] - itv["lo"] + 1)
                    score /= w(0, 2**(entry["Size"] * 8))
                    if (not scores[name]["wQC"]) or score < scores[name]["wQC"]:
                        scores[name]["wQC"] = score
    get_sns("S&S")
    get_sns("S&SFB")
    counts = domains.get_counts_from_symb(data, sink_filter = sink_filter)
    for entry in counts:
        name = maybe_make_entry(entry)
        if "Count" in entry.keys() and entry["Count"]:
            scores[name]["QC"] = res.log2(entry["Count"]) / (entry["Size"] * 8)
    return scores

def mk_basic_table(w = weight_inv):
    data = get_basic_scores(res.load(), w = w)
    table = []
    headers = ["Bug", "Sink", "Size", "QC", "wQC"]
    for _, entry in data.items():
        tabentry = []
        tabentry.append(entry["Bug"])
        tabentry.append(entry["Sink"])
        tabentry.append(entry["Size"] * 8)
        tabentry.append(entry["QC"])
        tabentry.append(entry["wQC"])
        table.append(tabentry)
    table = tab.tabulate(table, headers = headers, floatfmt = ".3g", tablefmt = res.fmt)
    print("Basic scores based on counts and domains:")
    print()
    print(table)

if __name__ == "__main__":
    res.help_msg("\n[SPECIFIC OPTIONS]")
    bars = res.opt_enable("-bars", "Display scores as a bar plot.")
    basic = res.opt_enable("-basic", "Display basic scores based on counts and domains, with the selected weight function (inv by default) for wQC (may not make sense for all sinks!!!).")
    wfun = res.opt_string("-weight-function", None, "Select a weight function for computing basic scores (inv, squareinv, sqrtinv).")
    merged = res.opt_enable("-merged", "Display merged scores.")
    simpl_labels = res.opt_string("-simpl-labels", None, "Command to simplify labels for the bar plot (ex: sed \"s/.../.../\").")
    res.help_msg("\nDisplay OOB capability scores for vulnerabilities.")
    res.options_done()
    if bars:
        mk_bars(merged, simpl_labels)
    elif basic:
        if wfun:
            match wfun:
                case "inv":
                    wfun = weight_inv
                case "squareinv":
                    wfun = weight_squareinv
                case "sqrtinv":
                    wfun = weight_sqrtinv
        else:
            wfun = weight_inv
        mk_basic_table(w = wfun)
    else:
        mk_table(merged)
