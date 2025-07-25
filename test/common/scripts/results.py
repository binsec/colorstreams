import json
import sys
import glob 
import matplotlib.pyplot as plt
import matplotlib.patches as mpatches
import re
import collections
import tabulate
import numpy as np
import time
import matplotlib

matplotlib.rcParams['pdf.fonttype'] = 42
matplotlib.rcParams['ps.fonttype'] = 42

try:
    idx = sys.argv.index("-help")
    do_help = True
except Exception as _:
    do_help = False

def print_help(opt, desc):
    if do_help:
        print(opt + ": " + desc)

def options_done():
    if do_help:
        exit()

def help_msg(msg):
    if do_help:
        print(msg)

help_msg("[GENERIC OPTIONS]")

def opt_enable(opt, desc):
    print_help(opt, desc)
    try:
        idx = sys.argv.index(opt)
        return True
    except Exception as _:
        return False

def opt_string(opt, default, desc):
    print_help(opt, desc)
    try:
        idx = sys.argv.index(opt)
        return sys.argv[idx + 1]
    except Exception as _:
        return default

filter_any = re.compile(".*")

def opt_regex_filter(opt, default, desc):
    print_help(opt, desc)
    try:
        idx = sys.argv.index(opt)
        s = sys.argv[idx + 1]
    except Exception as _:
        s = default
    if s:
        return re.compile(s)
    else:
        return filter_any

def opt_do(opt, f, desc):
    print_help(opt, desc)
    try:
        idx = sys.argv.index(opt)
        f()
    except Exception as _:
        pass

def log2(n):
    if n == 0:
        return 0
    try:
        return np.log2(n)
    except Exception as _:
        return n.bit_length() - 1

outs = opt_string("-out", "out", "Directories to search for results (format: dir1;dir2;...).").split(';')
dpi = opt_string("-dpi", None, "DPI for figures.")
if dpi:
    dpi = int(dpi)
fsize = opt_string("-fsize", None, "Font size for figures.")
if fsize:
    plt.rcParams.update({"font.size": int(fsize)})
save = opt_string("-save", None, "Save figures to a file with given path.")
legendloc = opt_string("-legend-loc", "lower right", "Location of legends (default: lower right).")
fmt = opt_string("-fmt", "simple", desc = "Select table formatter.")
title = not opt_enable("-nt", "Disables titles for figures.")
rfilter = opt_regex_filter("-filter-names", None, "Filter results by name (regex). Outputs have the following naming convention: name.tag.ext.")
tfilter = opt_regex_filter("-filter-tags", "^$", "Filter results by tag (regex). Outputs have the following naming convention: name.tag.ext. Default: only empty tags.")
sfilter = opt_regex_filter("-filter-sinks", None, "Filter sinks to display results for (regex).")

def load(outs = outs, rfilter = rfilter, tfilter = tfilter):
    files = [file for out in outs for file in glob.glob(out + "/*.json")]
    files.sort()
    files.reverse()
    data = collections.OrderedDict()
    for file in files:
        fname = file.split('/')[-1]
        toks = fname.split('.')
        name = toks[0]
        if len(toks) == 2:
            tag = ""
            tagged = name
        else:
            tag = toks[-2]
            tagged = ".".join(toks[:-1])
        with open(file) as file:
            if re.fullmatch(rfilter, name) and re.fullmatch(tfilter, tag):
                data[tagged] = json.load(file)
    return data

def get_maybe_z(s):
    if s.startswith("Z:"):
        return int(s[2:]), True
    else:
        return s, False

def show_or_save():
    if save:
        plt.savefig(save, dpi = dpi)
    else:
        plt.show()

def get_from(data, mk_entry, analysis_filter = filter_any, sink_filter = sfilter):
    res = []
    for prog, d in data.items():
        d = d["results"]
        for analysis in (k for k in d.keys() if re.fullmatch(analysis_filter, k)):
            for sink, sinkd in d[analysis]["result"].items():
                if re.fullmatch(sink_filter, sink) and ((not "Constraint" in sinkd["result"].keys()) or sinkd["result"]["Constraint"]["result"] == "sat"):
                    res.append(mk_entry(prog, analysis, sink, sinkd))
    return res

sns_splits = opt_string("-sns-splits", None, "Select max splits for S&S and its variants (format: max1;max2;...).")
if sns_splits:
    sns_splits = [int(s) for s in sns_splits.split(";")]

def get_sns_with_splits(sns, name = "S&S", sns_splits = sns_splits):
    d = sns["result"]["result"]["result"]
    res = collections.OrderedDict()
    if name in d.keys():
        d = d[name]["result"]
        def check_splits(k):
            if sns_splits:
                return any(k == "%d max splits" % splits for splits in sns_splits)
            else:
                return True
        keys = list(d.keys())
        def get_key(k):
            try:
                return int(k.split(" ")[0])
            except Exception as _:
                return k
        keys.sort(key = get_key)
        for snsk in keys:
            if check_splits(snsk):
                res[snsk] = d[snsk]
    return res

def get_basic_results(data, toget = ["Taint", "WeakControl", "StrongControl", "Count", "Newsome", "S&S", "S&SFB"], sink_filter = sfilter, sns_splits = sns_splits):
    def mk_symb_entry(prog, analysis, sink, sinkd):
        sinkd = sinkd["result"]
        entry = collections.OrderedDict()
        entry["Bug"] = prog
        entry["Analysis"] = analysis
        entry["Sink"] = sink
        entry["Size"] = sinkd["Sink"]["result"]["size"]
        for prop in toget:
            match prop:
                case "WeakControl":
                    if prop in sinkd.keys():
                        if sinkd[prop]["stats"]:
                            entry[prop] = sinkd[prop]["result"]
                        elif "result" in sinkd[prop]["result"].keys():
                            entry[prop] = sinkd[prop]["result"]["result"]["result"]
                        else:
                            entry[prop] = "error"
                case "StrongControl":
                    if "NotStrongControl" in sinkd.keys():
                        if "result" in sinkd["NotStrongControl"]["result"].keys():
                            nsc = sinkd["NotStrongControl"]["result"]["result"]["result"]
                            if nsc == True:
                                entry[prop] = True
                            elif nsc == False:
                                entry[prop] = False
                            elif "infeasible" in nsc.keys():
                                entry[prop] = False
                        else:
                            entry[prop] = "error"
                    elif "StrongControl" in sinkd.keys():
                        if "result" in sinkd[prop]["result"].keys():
                            entry[prop] = sinkd["StrongControl"]["result"]["result"]["result"]
                        else:
                            entry[prop] = "error"
                case "Count":
                    if prop in sinkd.keys():
                        countd = sinkd[prop]["result"]["result"]["result"]
                        entry[prop] = {}
                        for countk in countd.keys():
                            entry[prop][countk], _ = get_maybe_z(countd[countk]["result"])
                case "Newsome":
                    if prop in sinkd.keys():
                        l = sinkd[prop]["result"]["result"]["result"]["Total"]["result"]["probabilistic"]["result"]
                        t = sinkd[prop]["result"]["result"]["stats"]["timeout"]
                        entry[prop] = int(l[0][2:]), int(l[-1][2:]), t
                case "S&S" | "S&SFB":
                    if "S&S" in sinkd.keys():
                        entry[prop] = collections.OrderedDict()
                        for snsk, resd in get_sns_with_splits(sinkd["S&S"], name = prop, sns_splits = sns_splits).items():
                            lo, _ = get_maybe_z(resd["result"]["Count"]["result"]["min"]["result"])
                            hi, _ = get_maybe_z(resd["result"]["Count"]["result"]["max"]["result"])
                            t = resd["stats"]["timeout"]
                            entry[prop][snsk] = (lo, hi, t)
        return entry
    symb_entries = get_from(data, mk_symb_entry, analysis_filter = re.compile(".*Symbolic.*"), sink_filter = sink_filter)
    if "Taint" in toget:
        def mk_taint_entry(prog, analysis, sink, sinkd):
            sinkd = sinkd["result"]
            entry = {}
            entry["Bug"] = prog
            entry["Analysis"] = analysis
            entry["Sink"] = sink
            if "Taint" in sinkd.keys():
                entry["Taint"] = any((isinstance(tt, str) and tt.startswith("D")) or ((not isinstance(tt, str)) and "0" in tt.keys()) for sto, stod in sinkd["Taint"]["result"].items() for tt in stod["result"])
            return entry
        taint_entries = get_from(data, mk_taint_entry, analysis_filter = re.compile(".*Byte Dep.*"), sink_filter = sink_filter)
        for entry in symb_entries:
            l = [te for te in taint_entries if te["Bug"] == entry["Bug"] and te["Sink"] == entry["Sink"] and "Taint" in te.keys()]
            if l != []:
                entry["Taint"] = l[0]["Taint"]
    return symb_entries

def get_runtimes(data, algos, sink_filter = sfilter, sns_splits = sns_splits, timeout = False):
    def mk_entry(prog, analysis, sink, sinkd):
        sinkd = sinkd["result"]
        entry = {}
        entry["Bug"] = prog
        entry["Analysis"] = analysis
        entry["Sink"] = sink
        for k in algos:
            if k.startswith("S&S") and "S&S" in sinkd.keys():
                entry[k] = {}
                for snsk, resd in get_sns_with_splits(sinkd["S&S"], name = k, sns_splits = sns_splits).items():
                    entry[k][snsk] = resd["stats"]["runtime"]
            elif k in sinkd.keys():
                if k == "Count":
                    countd = sinkd[k]["result"]["result"]["result"]
                    entry[k] = {}
                    for countk in countd.keys():
                        if timeout or countd[countk]["result"] != "timeout":
                            entry[k][countk] = countd[countk]["stats"]["runtime"]
                elif sinkd[k]["stats"]:
                    entry[k] = sinkd[k]["stats"]["runtime"]
                elif "result" in sinkd[k]["result"].keys() and sinkd[k]["result"]["result"]["stats"]:
                    entry[k] = sinkd[k]["result"]["result"]["stats"]["runtime"]
                elif timeout and "error" in sinkd[k]["result"].keys() and sinkd[k]["result"]["error"]["stats"]:
                    entry[k] = sinkd[k]["result"]["error"]["stats"]["runtime"]
                else:
                    entry[k] = None
            else:
                if k == "SE":
                    entry[k] = sinkd["Symbolic"]["stats"]["se_runtime"]
                elif k == "Tracing":
                    entry[k] = sinkd["Symbolic"]["stats"]["runtime"]
                else:
                    entry[k] = None
        return entry
    return get_from(data, mk_entry, analysis_filter = re.compile(".*Symbolic.*"), sink_filter = sink_filter)

def mk_results_comp(data, sink_filter = sfilter, sns_splits = sns_splits, only_controlled = False):
    results = collections.OrderedDict()
    def mk_entry():
        return {">": 0, "=": 0, "<": 0, "NA": 0}
    for entry in get_basic_results(data, sink_filter = sink_filter, sns_splits = sns_splits):
        maxc = 2 ** (entry["Size"] * 8)
        if "Count" in entry.keys() and "d4" in entry["Count"].keys() and entry["Count"]["d4"] != "timeout":
            quant_gt = entry["Count"]["d4"]
        elif "Count" in entry.keys() and "ganak" in entry["Count"].keys() and entry["Count"]["ganak"] != "timeout":
            quant_gt = entry["Count"]["ganak"]
        else:
            quant_gt = None
        if "WeakControl" in entry.keys() and not entry["WeakControl"] == "error":
            wc_gt = entry["WeakControl"]
        else:
            wc_gt = None
        if wc_gt == False and only_controlled:
            continue
        for sns in [k for k in entry.keys() if k.startswith("S&S")]:
            for splits in entry[sns].keys():
                name = sns + " (" + splits + ")"
                if not name in results.keys():
                    results[name] = {"Taint": mk_entry()}
                lo, hi, t = entry[sns][splits]
                for k in entry.keys():
                    match k:
                        case "Taint" | "WeakControl":
                            if not k in results[name]:
                                results[name][k] = mk_entry()
                            wc = entry[k]
                            if wc_gt != None:
                                if wc_gt:
                                    if wc == "error":
                                        results[name][k][">"] += 1
                                    elif wc:
                                        if lo > 1:
                                            if lo > 2 or hi < maxc:
                                                results[name][k][">"] += 1
                                            else:
                                                results[name][k]["="] += 1
                                        else:
                                            results[name][k]["<"] += 1
                                    else:
                                        if lo > 1:
                                            results[name][k][">"] += 1
                                        else:
                                            results[name][k]["="] += 1
                                else:
                                    if wc:
                                        if hi < 2:
                                            results[name][k][">"] += 1
                                        else:
                                            results[name][k]["="] += 1
                                    else:
                                        if hi < 2:
                                            results[name][k]["="] += 1
                                        else:
                                            results[name][k]["<"] += 1
                            else:
                                results[name][k]["NA"] += 1
                        case "StrongControl":
                            if not k in results[name]:
                                results[name][k] = mk_entry()
                            if entry[k] == "error":
                                results[name][k][">"] += 1
                            elif (entry[k] and lo == maxc) or (entry[k] == False and hi == maxc - 1 and lo == 1):
                                results[name][k]["="] += 1
                            elif (not entry[k]) and hi < maxc:
                                results[name][k][">"] += 1
                            elif not entry[k] == None:
                                results[name][k]["<"] += 1
                            else:
                                results[name][k]["NA"] += 1
                        case "Count":
                            for pmc in entry[k]:
                                if not pmc in results[name]:
                                    results[name][pmc] = mk_entry()
                                if entry[k][pmc] == "timeout":
                                    results[name][pmc][">"] += 1
                                else:
                                    count = entry[k][pmc]
                                    if pmc == "approxmc":
                                        if quant_gt:
                                            dapproxmc = abs(quant_gt - entry[k][pmc])
                                            dsns = abs(quant_gt - lo) + abs(quant_gt - hi)
                                            if dapproxmc < dsns:
                                                results[name][pmc]["<"] += 1
                                            elif dapproxmc == dsns:
                                                results[name][pmc]["="] += 1
                                            else:
                                                results[name][pmc][">"] += 1
                                        else:
                                            results[name][pmc]["NA"] += 1
                                    elif pmc == "d4" or pmc == "ganak":
                                        if lo == hi and lo == count:
                                            results[name][pmc]["="] += 1
                                        else: 
                                            results[name][pmc]["<"] += 1
                        case "Newsome":
                            if not k in results[name]:
                                results[name][k] = mk_entry()
                            lon, hin, tn = entry[k]
                            if lo < lon and hi > hin:
                                results[name][k]["<"] += 1
                            elif lo > lon and hi < hin:
                                results[name][k][">"] += 1
                            else:
                                dn = hin - lon
                                dsns = hi - lo
                                if dsns < dn:
                                    results[name][k][">"] += 1
                                elif dsns == dn:
                                    results[name][k]["="] += 1
                                else:
                                    results[name][k]["<"] += 1
    return results

def mk_results_short(data, sink_filter = sfilter, sns_splits = sns_splits, approx_margin = 0.05):
    results = collections.OrderedDict()
    for analysis in ["Taint", "WeakControl", "StrongControl"]:
        results[analysis] = {"TP": 0, "TN": 0, "FP": 0, "FN": 0, "TO": 0, "R": 0}
    for analysis in ["d4", "ganak"]:
        results[analysis] = {"T": 0, "TO": 0, "R": 0}
    for analysis in ["approxmc", "Newsome"]:
        results[analysis] = {"T": 0, "<": 0, ">": 0, "U": 0, "TO": 0, "R": 0}
    def check_approx(gt, cnt):
        return abs(gt - cnt) / gt < approx_margin
    def check_approx_itv(gt, lo, hi):
        return check_approx(gt, lo) and check_approx(gt, hi)
    for entry in get_basic_results(data, sink_filter = sink_filter, sns_splits = sns_splits):
        if "Count" in entry.keys() and "d4" in entry["Count"].keys() and entry["Count"]["d4"] != "timeout":
            quant_gt = entry["Count"]["d4"]
        elif "Count" in entry.keys() and "ganak" in entry["Count"].keys() and entry["Count"]["ganak"] != "timeout":
            quant_gt = entry["Count"]["ganak"]
        else:
            quant_gt = None
        if "WeakControl" in entry.keys() and not entry["WeakControl"] == "error":
            wc_gt = entry["WeakControl"]
        else:
            wc_gt = None
        for k in entry.keys():
            match k:
                case "Taint" | "WeakControl":
                    if wc_gt != None:
                        if entry[k] == "error":
                            results[k]["TO"] += 1
                        elif entry[k] == wc_gt:
                            if entry[k]:
                                results[k]["TP"] += 1
                            else:
                                results[k]["TN"] += 1
                        else:
                            if entry[k]:
                                results[k]["FP"] += 1
                            else:
                                resutls[k]["FN"] += 1
                case "StrongControl":
                    if entry[k] == "error":
                        results[k]["TO"] += 1
                    elif entry[k]:
                        results[k]["TP"] += 1
                    else:
                        results[k]["TN"] += 1
                case "Count":
                    for pmc in ["d4", "ganak"]:
                        if pmc in entry[k].keys():
                            if entry[k][pmc] != "timeout":
                                results[pmc]["T"] += 1
                            else:
                                results[pmc]["TO"] += 1
                    if "approxmc" in entry[k].keys():
                        if entry[k]["approxmc"] == "timeout":
                            results["approxmc"]["TO"] += 1
                        elif quant_gt:
                            if entry[k]["approxmc"] == quant_gt:
                                results["approxmc"]["T"] += 1
                            elif check_approx(quant_gt, entry[k]["approxmc"]):
                                results["approxmc"]["<"] += 1
                            else:
                                results["approxmc"][">"] += 1
                        else:
                            results["approxmc"]["U"] += 1
                case "Newsome":
                    lo, hi, t = entry[k]
                    if quant_gt:
                        if lo == hi and lo <= 64 and lo == quant_gt:
                            results["Newsome"]["T"] += 1
                        elif check_approx_itv(quant_gt, lo, hi):
                            results["Newsome"]["<"] += 1
                        else:
                            results["Newsome"][">"] += 1
                    else:
                        results["Newsome"]["U"] += 1
                    if t:
                        results["Newsome"]["TO"] += 1
                case _:
                    if k.startswith("S&S"):
                        for splits in entry[k].keys():
                            name = k + " (" + splits + ")"
                            if not (name in results.keys()):
                                results[name] = {"T": 0, "<": 0, ">": 0, "U": 0, "TO": 0, "R": 0}
                            lo, hi, t = entry[k][splits]
                            if quant_gt:
                                if lo == hi and lo == quant_gt:
                                    results[name]["T"] += 1
                                elif check_approx_itv(quant_gt, lo, hi):
                                    results[name]["<"] += 1
                                else:
                                    results[name][">"] += 1
                            else:
                                results[name]["U"] += 1
                            if t:
                                results[name]["TO"] += 1
    runtimes = get_runtimes(data, ["WeakControl", "NotStrongControl", "Count", "Newsome", "S&S", "S&SFB"], sink_filter = sink_filter, sns_splits = sns_splits, timeout = True)
    for entry in runtimes:
        for k in entry.keys():
            if k == "Count":
                for pmc in entry[k].keys():
                    if pmc in results:
                        results[pmc]["R"] += entry[k][pmc]
            elif k == "NotStrongControl":
                results["StrongControl"]["R"] += entry[k]
            elif k.startswith("S&S"):
                for splits in entry[k]:
                    sns = "%s (%s)" % (k, splits)
                    if sns in results:
                        results[sns]["R"] += entry[k][splits]
            elif k in results.keys():
                results[k]["R"] += entry[k]
    return results

if __name__ == "__main__":
    help_msg("\n[SPECIFIC OPTIONS]")
    raw = opt_enable("-raw", "Print raw json.")
    verbose = opt_enable("-verbose", "Print detailed results.")
    comp = opt_enable("-comp", "Print comparative results.")
    compbars = opt_enable("-compbars", "Show comparative results as histograms.")
    compbars_qc = opt_enable("-compbars-qc", "Only show QC comparative results.")
    compbars_doc = opt_enable("-compbars-domains", "Only show domain comparative results.")
    compbars_control_discr = opt_enable("-compbars-control-discr", "Discriminate comparative results for domains base on control.")
    selected = opt_string("-select", "Taint;WeakControl;StrongControl;Count;Newsome;S&S;S&SFB", desc = "Select detailed results to display (default: Taint;WeakControl;StrongControl;Count;Newsome;S&S;S&SFB).").split(";")
    pmc_solvers = opt_string("-pmc-solvers", "d4;ganak;approxmc", "Select PMC solvers to display detailed results for (default: d4;ganak;approxmc).").split(";")
    show_analysis = opt_enable("-show-analysis", "Show which analysis detailed results came from.")
    approx_margin = float(opt_string("-approx-margin", "1", "Precision margin for approximate results (default: 1)."))
    help_msg("\nDisplay results.")
    options_done()

    if raw:
        print(json.dumps(load(), indent = 4))
    elif verbose:
        table = []
        if show_analysis:
            selected = ["Bug", "Analysis", "Sink"] + selected
        else:
            selected = ["Bug", "Sink"] + selected
        headers = []
        if sns_splits:
            sns_splits_ = ["%d max splits" % splits for splits in sns_splits]
        else:
            sns_splits_ = []
        for entry in get_basic_results(load(), sns_splits = sns_splits):
            table.append([])
            for s in selected:
                match s:
                    case "Count":
                        for pmc in pmc_solvers:
                            if not pmc in headers:
                                headers.append(pmc)
                            if s in entry.keys() and pmc in entry[s].keys():
                                table[-1].append(entry[s][pmc])
                            else:
                                table[-1].append("-")
                    case "S&S" | "S&SFB":
                        if not sns_splits:
                            if s in entry.keys():
                                for splits in entry[s].keys():
                                    if not splits in sns_splits_:
                                        sns_splits_.append(splits)
                        for splits in sns_splits_:
                            name = s + " ("+ splits + ")"
                            if not name in headers:
                                headers.append(name)
                            if splits in entry[s].keys():
                                lo, hi, t = entry[s][splits]
                                if t:
                                    suffix = " (T)"
                                else:
                                    suffix = ""
                                if lo == hi:
                                    if t:
                                        lo = "%d%s" % (lo, suffix)
                                    table[-1].append(lo)
                                else:
                                    table[-1].append("%d - %d%s" % (lo, hi, suffix))
                            else:
                                table[-1].append("-")
                    case "Newsome":
                        if not s in headers:
                            headers.append(s)
                        if s in entry.keys():
                            lo, hi, t = entry[s]
                            if t:
                                suffix = " (T)"
                            else:
                                suffix = ""
                            if lo == hi and lo <= 64:
                                if t:
                                    lo = "%d%s" % (lo, suffix)
                                table[-1].append(lo)
                            else:
                                table[-1].append("%d - %d%s" % (lo, hi, suffix))
                    case _:
                        if not s in headers:
                            headers.append(s)
                        if s in entry.keys():
                            table[-1].append(entry[s])
                        else:
                            table[-1].append("-")
        table.reverse()
        colalign = []
        for i in range(len(headers)):
            colalign.append("left")
            if any(isinstance(table[j][i], int) and not isinstance(table[j][i], bool) for j in range(len(table))):
                colalign[-1] = "right"
                continue
        print("Results:")
        print()
        print(tabulate.tabulate(table, headers = headers, tablefmt = fmt, colalign = colalign))
    elif comp:
        table = []
        headers = [""]
        results = mk_results_comp(load(), sns_splits = sns_splits)
        n = 0
        for sns, snsd in results.items():
            entry = [sns]
            for algo in snsd.keys():
                if not algo in headers:
                    headers.append(algo)
                entry.append("%d / %d / %d / %d" % (snsd[algo][">"], snsd[algo]["="], snsd[algo]["<"], snsd[algo]["NA"]))
                n = max(n, snsd[algo][">"] + snsd[algo]["="] + snsd[algo]["<"] + snsd[algo]["NA"])
            table.append(entry)
        print("S&S results compared to other algorithms (> / = / < / NA, %d total):" % n)
        print()
        print(tabulate.tabulate(table, headers = headers, tablefmt = fmt))
    elif compbars:
        results = mk_results_comp(load(), sns_splits = sns_splits, only_controlled = False)
        results_ = mk_results_comp(load(), sns_splits = sns_splits, only_controlled = True)
        if compbars_doc or compbars_qc:
            figure = plt.figure(dpi = dpi, constrained_layout = True, figsize = (6.4, 4.8 * 7 / 12))
            figs = figure.subfigures(nrows = 3, height_ratios = [1, 5, 1])
            axs = [figs[1].subplots(ncols=len(results))]
        else:
            figure = plt.figure(dpi = dpi, constrained_layout = True)
            figs = figure.subfigures(nrows = 4, height_ratios = [5, 5, 1, 1])
            axs = [fig.subplots(ncols = len(results)) for fig in figs[0:2]]
        short = {"WeakControl": "WC", "StrongControl": "SC"}
        def mk_short(k):
            if k in short.keys():
                return short[k]
            else:
                return k
        def mk_dom_comp(ax, snsn, snsd, algos, only_controlled = False):
            x = np.arange(len(algos))
            better = [snsd[algo][">"] for algo in algos]
            eq = [snsd[algo]["="] for algo in algos]
            worse = [snsd[algo]["<"] for algo in algos]
            na = [snsd[algo]["NA"] for algo in algos]
            def mk_bar(shift, data, label, color):
                if not compbars_control_discr:
                    ax.bar_label(ax.bar(x + shift, data, width = 0.2, label = label, color = color), rotation = "vertical", padding = 3)
                elif only_controlled:
                    bars = ax.bar(x + shift, data, width = 0.2, label = label, color = color)
                else:
                    ax.bar_label(ax.bar(x + shift, data, width = 0.2, label = "_" + label, fill = False, edgecolor = color), rotation = "vertical", padding = 3)
            mk_bar(-0.3, better, "S&S better", "lightgreen")
            mk_bar(0, eq, "Equal", "lightblue")
            mk_bar(0.3, worse, "S&S worse", "pink")
            #ax.bar_label(ax.bar(x - 0.3, better, width = 0.2, label = "S&S better", color = "lightgreen"), rotation = "vertical", padding = 3)
            #ax.bar_label(ax.bar(x, eq, width = 0.2, label = "Equal", color = "lightblue"), rotation = "vertical", padding = 3)
            #ax.bar_label(ax.bar(x + 0.3, worse, width = 0.2, label = "S&S worse", color = "pink"), rotation = "vertical", padding = 3)
            ax.set_yticks([])
            if not only_controlled:
                bot, top = ax.get_ylim()
                ax.set_ylim(bot, 1.4 * top)
            ax.set_xticklabels(["aaa"] + [mk_short(k) for k in algos])
        if compbars_doc and not compbars_qc:
            i = 0
            for snsn, snsd in results.items():
                mk_dom_comp(axs[0][i], snsn, snsd, ["Taint", "WeakControl", "StrongControl", "Newsome"])
                i += 1
            if compbars_control_discr:
                i = 0
                for snsn, snsd in results_.items():
                    mk_dom_comp(axs[0][i], snsn, snsd, ["Taint", "WeakControl", "StrongControl", "Newsome"], only_controlled = True)
                    i += 1
        elif not (compbars_qc or compbars_doc):
            i = 0
            for snsn, snsd in results.items():
                mk_dom_comp(axs[1][i], snsn, snsd, ["Taint", "WeakControl", "StrongControl", "Newsome"])
                i += 1
            if compbars_control_discr:
                i = 0
                for snsn, snsd in results_.items():
                    mk_dom_comp(axs[1][i], snsn, snsd, ["Taint", "WeakControl", "StrongControl", "Newsome"], only_controlled = True)
                    i += 1
        def mk_quant_comp(ax, snsn, snsd, algos):
            x = np.arange(len(algos))
            better = [snsd[algo][">"] for algo in algos]
            eq = [snsd[algo]["="] for algo in algos]
            worse = [snsd[algo]["<"] for algo in algos]
            na = [snsd[algo]["NA"] for algo in algos]
            ax.bar_label(ax.bar(x - 0.3, better, width = 0.15, label = "S&S better", color = "lightgreen"), rotation = 90, padding = 3)
            ax.bar_label(ax.bar(x - 0.1, eq, width = 0.15, label = "Equal", color = "lightblue"), rotation = 90, padding = 3)
            ax.bar_label(ax.bar(x + 0.1, worse, width = 0.15, label = "S&S worse", color = "pink"), rotation = 90, padding = 3)
            ax.bar_label(ax.bar(x + 0.3, na, width = 0.15, label = "Not comparable", color = "lightgrey"), rotation = 90, padding = 3)
            ax.set_yticks([])
            bot, top = ax.get_ylim()
            ax.set_ylim(bot, 1.4 * top)
            ax.set_xticks(x)
            ax.set_xticklabels(algos)
        i = 0
        if compbars_qc or not compbars_doc:
            for snsn, snsd in results.items():
                mk_quant_comp(axs[0][i], snsn, snsd, ["d4", "ganak", "approxmc"])
                i += 1
        if compbars_qc or compbars_doc:
            titlefigs = figs[0].subfigures(ncols=len(results))
        else:
            titlefigs = figs[-2].subfigures(ncols=len(results))
        for snsn, i in zip(results.keys(), range(len(results))):
            titlefigs[i].suptitle("(%d) %s" % (i + 1, snsn))
        if (not (compbars_qc or compbars_doc)):
            figs[0].suptitle("QC comparison")
            figs[1].suptitle("DoC comparison")
        handles, labels = axs[0][0].get_legend_handles_labels()
        if compbars_control_discr and compbars_doc and not compbars_qc:
            no_contr = mpatches.Patch(fill = False, edgecolor = "lightgrey", label = "no control")
            handles.append(no_contr)
            labels.append("no control")
        figs[-1].legend(handles, labels, loc = "lower center", ncols = 4, frameon = False, columnspacing = 1)
        figure.supylabel("# of problems")
        if title:
            if compbars_qc:
                figure.suptitle("QC precision comparison between S&S and other algorithms")
            elif compbars_doc:
                figure.suptitle("Domain precision comparison between S&S and other algorithms")
            else:
                figure.suptitle("Precision comparison between S&S and other algorithms")
        show_or_save()
    else:
        table = []
        headers = ["Algorithm", "True (TP/TN)", "Approx (<%.3g%%/>%.3g%%)" % (approx_margin * 100, approx_margin * 100), "False (FP/FN)", "Timeout", "Unknown", "Total Runtime"]
        align = ["left", "center", "center", "center", "right", "right"]
        results = mk_results_short(load(), sns_splits = sns_splits, approx_margin = approx_margin)
        def mk_time(s):
            h = int(s / 3600)
            m = int((s - h * 3600) / 60)
            s = int(s - h * 3600 - m * 60)
            if h == 0 and m == 0:
                return "%ds" % s
            elif h == 0:
                return "%dm%02ds" % (m, s)
            else:
                return "%dh%02dm%02ds" % (h, m, s)
        for analysis in ["Taint", "WeakControl", "StrongControl"]:
            row = [
                    analysis, 
                    "%d/%d" % (results[analysis]["TP"], results[analysis]["TN"]),
                    "-",
                    "%d/%d" % (results[analysis]["FP"], results[analysis]["FN"]),
                    "%d" % results[analysis]["TO"],
                    "-",
                    mk_time(results[analysis]["R"])
                    ]
            table.append(row)
        for analysis in ["d4", "ganak"]:
            row = [
                    analysis,
                    "%d" % results[analysis]["T"],
                    "-",
                    "-",
                    "%d" % results[analysis]["TO"],
                    "-",
                    mk_time(results[analysis]["R"])
                    ]
            table.append(row)
        sns = [k for k in results.keys() if k.startswith("S&S")]
        for analysis in ["approxmc", "Newsome"] + sns:
            row = [
                    analysis,
                    "%d" % results[analysis]["T"],
                    "%d/%d" % (results[analysis]["<"], results[analysis][">"]),
                    "-",
                    "%d" % results[analysis]["TO"],
                    "%d" % results[analysis]["U"],
                    mk_time(results[analysis]["R"])
                    ]
            table.append(row)
        print("Results compared to ground truth (d4 or ganak, when available):")
        print()
        print(tabulate.tabulate(table, headers = headers, tablefmt = fmt, colalign = align))
