import matplotlib.pyplot as plt
import matplotlib.ticker as tck
import results as res
import re
import numpy as np

itvexp = re.compile(r"I[0-9]+")
sink_filter = res.sfilter

def get_sns(dom):
    res = []
    for k, d in dom["result"].items():
        if re.fullmatch(itvexp, k):
            itv = {}
            itv["status"] = d["result"]["status"]["result"]
            itv["lo"] = int(d["result"]["interval"]["result"]["lo"][2:])
            itv["hi"] = int(d["result"]["interval"]["result"]["hi"][2:]) - 1
            itv["card_lo"] = int(d["result"]["interval"]["result"]["card_lo"][2:])
            itv["card_hi"] = int(d["result"]["interval"]["result"]["card_hi"][2:])
            res.append(itv)
    return res

def get_sns_from_symb(data, name = "S&S", splits = res.sns_splits, sink_filter = sink_filter):
    def mk_entry(prog, analysis, sink, sinkd):
        sinkd = sinkd["result"]
        results = {}
        if "S&S" in sinkd.keys():
            for snsk, snsd in res.get_sns_with_splits(sinkd["S&S"], name, splits).items():
                entry = {}
                entry["Bug"] = prog
                entry["Analysis"] = analysis
                entry["Sink"] = sink
                entry["Size"] = sinkd["Sink"]["result"]["size"]
                entry["Domain"] = get_sns(snsd)
                results[snsk] = entry
        return results
    entries = res.get_from(data, mk_entry, analysis_filter = re.compile(".*Symbolic.*"), sink_filter = sink_filter)
    results = {}
    for entry in entries:
        for splits, e in entry.items():
            if not splits in results.keys():
                results[splits] = []
            results[splits].append(e)
    return results

def get_wc_sc_from_symb(data, sink_filter = sink_filter):
    def mk_entry(prog, analysis, sink, sinkd):
        sinkd = sinkd["result"]
        entry = {}
        entry["Bug"] = prog
        entry["Analysis"] = analysis
        entry["Sink"] = sink
        entry["Size"] = sinkd["Sink"]["result"]["size"]
        if "WeakControl" in sinkd.keys():
            wcd = sinkd["WeakControl"]
            if wcd["stats"]:
                entry["WC"] = wcd["result"]
            elif "result" in wcd["result"].keys():
                entry["WC"] = wcd["result"]["result"]["result"]
            else:
                entry["WC"] = None
        else:
            entry["WC"] = None
        if "NotStrongControl" in sinkd.keys() and "result" in sinkd["NotStrongControl"]["result"].keys():
            nsc = sinkd["NotStrongControl"]["result"]["result"]["result"]
            if nsc == True:
                entry["SC"] = True
            elif nsc == False:
                entry["SC"] = False
            elif "infeasible" in nsc.keys():
                entry["SC"] = False
        elif "StrongControl" in sinkd.keys() and "result" in sinkd["StrongControl"]["result"].keys():
            entry["SC"] = sinkd["StrongControl"]["result"]["result"]["result"]
        else:
            entry["SC"] = None
        return entry
    return res.get_from(data, mk_entry, analysis_filter = re.compile(".*Symbolic.*"), sink_filter = sink_filter)

def get_taint(data, sink_filter = sink_filter):
    def mk_entry(prog, analysis, sink, sinkd):
        sinkd = sinkd["result"]
        entry = {}
        entry["Bug"] = prog
        entry["Analysis"] = analysis
        entry["Sink"] = sink
        entry["Size"] = sinkd["Sink"]["result"]["size"]
        if "Taint" in sinkd.keys():
            td = sinkd["Taint"]["result"]
            for sto, stod in td.items():
                #note: accounts for taint from multiple sources
                if any((isinstance(tt, str) and tt.startswith("D")) or ((not isinstance(tt, str)) and "0" in tt.keys()) for tt in stod["result"]):
                    entry["Taint"] = True
                    break
        if not ("Taint" in entry.keys()):
            entry["Taint"] = False
        return entry
    return res.get_from(data, mk_entry, analysis_filter = re.compile(".*Byte Dep.*"), sink_filter = sink_filter)

def get_counts_from_symb(data, sink_filter = sink_filter):
    def mk_entry(prog, analysis, sink, sinkd):
        sinkd = sinkd["result"]
        entry = {}
        entry["Bug"] = prog
        entry["Analysis"] = analysis
        entry["Sink"] = sink
        entry["Size"] = sinkd["Sink"]["result"]["size"]
        if "Count" in sinkd.keys():
            cd = sinkd["Count"]["result"]["result"]["result"]
            for pmc in ["d4", "ganak", "approxmc"]:
                if pmc in cd.keys():
                    cnt, ok = res.get_maybe_z(cd[pmc]["result"])
                    if ok:
                        entry["Count"] = cnt
                        break
        return entry
    return res.get_from(data, mk_entry, analysis_filter = re.compile(".*Symbolic.*"), sink_filter = sink_filter)

def draw(ax, entries, sizes, labels, colors, itv_entries, itv_los, itv_diffs, heights = None):
    cnt = 0
    idx = {}
    maxsize = 0
    for entry, size in zip(entries, sizes):
        if not entry in idx.keys():
            idx[entry] = cnt
            ax.plot([0, size * 8], [entry, entry], color = "black", linestyle = ":")
            cnt += 1
            maxsize = max(maxsize, size * 8)
    drawn = False
    for label in labels:
        if itv_entries[label] != []:
            drawn = True
            #idxs = [idx[entry] for entry in itv_entries[label]]
            if heights:
                height = heights[label]
            else:
                height = 0.8
            ax.barh(itv_entries[label], itv_diffs[label], left = itv_los[label], height = height, label = label, color = colors[label])
    ax.set_xlim(left = -1, right = maxsize + 1)
    #ax.set_yticks(np.arange(len(entries)), entries, ha = "right")
    ax.xaxis.set_major_locator(tck.MultipleLocator(base = 16))
    ax.xaxis.set_major_formatter(tck.FormatStrFormatter("2$^{%d}$"))
    if drawn:
        ax.legend(loc = res.legendloc, ncols = 1)

def mk_name(entry):
    return entry["Bug"] + "." + entry["Sink"]

def draw_qc(ax, counts):
    maxsize = 0
    for entry in counts:
        name = mk_name(entry)
        size = entry["Size"] * 8
        ax.plot([0, size], [name, name], color = "black", linestyle = ":")
        maxsize = max(size, maxsize)
        if "Count" in entry.keys():
            qc = res.log2(entry["Count"]) / size
            ax.barh(name, size, height = 0.8, color = (1 - qc, 1 - qc, 1 - qc), edgecolor = "black", linewidth = 1)
    ax.set_xlim(left = -1, right = maxsize + 1)
    ax.xaxis.set_major_locator(tck.MultipleLocator(base = 16))
    ax.xaxis.set_major_formatter(tck.FormatStrFormatter("2$^{%d}$"))

def draw_sns(ax, domains):
    labels = ["weak", "strong"]
    colors = {"weak": "lightgrey", "strong": "black"}
    heights = {"weak": 0.8, "strong": 0.6}
    entries, sizes = [], []
    itv_entries, itv_los, itv_diffs = {}, {}, {}
    for label in labels:
        itv_entries[label] = []
        itv_los[label] = []
        itv_diffs[label] = []
    for entry in domains:
        name = mk_name(entry)
        entries.append(name)
        sizes.append(entry["Size"])
        if entry["Domain"]:
            for itv in entry["Domain"]:
                status = itv["status"]
                if status == "maybe strong":
                    status = "weak"
                if status in labels:
                    itv_entries[status].append(name)
                    lo = res.log2(itv["lo"])
                    hi = res.log2(itv["hi"])
                    if hi - lo == 0:
                        hi = hi + 0.25
                        lo = lo - 0.25
                    elif hi - lo < 1:
                        diff = 1 - hi + lo
                        hi = hi + diff / 2
                        lo = lo - diff / 2
                    itv_los[status].append(lo)
                    itv_diffs[status].append(hi - lo)
    draw(ax, entries, sizes, labels, colors, itv_entries, itv_los, itv_diffs, heights = heights)

def draw_wc_sc(ax, wc_sc):
    labels = ["weak", "strong"]
    colors = {"weak": "lightgrey", "strong": "black"}
    entries, sizes = [], []
    itv_entries, itv_los, itv_diffs = {}, {}, {}
    for label in labels:
        itv_entries[label] = []
        itv_los[label] = []
        itv_diffs[label] = []
    for entry in wc_sc:
        name = mk_name(entry)
        entries.append(name)
        sizes.append(entry["Size"])
        if entry["SC"]:
            itv_entries["strong"].append(name)
            itv_los["strong"].append(0)
            itv_diffs["strong"].append(entry["Size"] * 8)
        elif entry["WC"]:
            itv_entries["weak"].append(name)
            itv_los["weak"].append(0)
            itv_diffs["weak"].append(entry["Size"] * 8)
    draw(ax, entries, sizes, labels, colors, itv_entries, itv_los, itv_diffs)

def draw_taint(ax, taint):
    labels = ["tainted"]
    colors = {"tainted": "black"}
    entries, sizes = [], []
    itv_entries, itv_los, itv_diffs = {"tainted": []}, {"tainted": []}, {"tainted": []}
    for entry in taint:
        name = mk_name(entry)
        entries.append(name)
        sizes.append(entry["Size"])
        if entry["Taint"]:
            itv_entries["tainted"].append(name)
            itv_los["tainted"].append(0)
            itv_diffs["tainted"].append(entry["Size"] * 8)
    draw(ax, entries, sizes, labels, colors, itv_entries, itv_los, itv_diffs)

def mk_plot(data, todo, nolabels = False):
    fig = plt.figure(dpi = res.dpi)
    ncols = 0
    sns = {}
    sinks = {}
    for algo in todo:
        if algo.startswith("S&S"):
            sns[algo] = get_sns_from_symb(data, name = algo)
            for snsk, snsd in sns[algo].items():
                for entry in snsd:
                    sinks[entry["Bug"] + entry["Sink"]] = True
            ncols += len(list(sns[algo].keys()))
        else:
            ncols += 1
    axs = fig.subplots(1, ncols, sharey = True)
    if ncols == 1:
        axs = [axs]
    i = 0
    for algo in todo:
        match algo:
            case "S&S" | "S&SFB":
                for splits, entries in sns[algo].items():
                    draw_sns(axs[i], entries)
                    axs[i].set_title("(%d) %s (%s)" % ((i + 1), algo, splits))
                    i += 1
            case "QC":
                draw_qc(axs[i], get_counts_from_symb(data))
                axs[i].set_title(("(%d) " % (i + 1)) + algo)
                i += 1
            case "WCSC":
                draw_wc_sc(axs[i], get_wc_sc_from_symb(data))
                axs[i].set_title(("(%d) " % (i + 1)) + algo)
                i += 1
            case "Taint":
                draw_taint(axs[i], [entry for entry in get_taint(data) if entry["Bug"] + entry["Sink"] in sinks.keys()])
                axs[i].set_title(("(%d) " % (i + 1)) + algo)
                i += 1
    if nolabels:
        for ax in axs:
            ax.set_yticklabels([])
    if not nolabels:
        fig.supxlabel("value range")
        fig.supylabel("target")
    if axs[0].get_yticklabels() != []:
        maxlabel = max(label.get_window_extent().width for label in axs[0].get_yticklabels())
        fsize = axs[0].get_yticklabels()[0].get_fontsize()
    else:
        maxlabel = 0
        fsize = 10
    fig.set_size_inches(maxlabel / fig.dpi + 2.2 * len(axs) + 1, fsize / 72 * len(axs[0].get_yticks()) + 2)
    fig.tight_layout()
    res.show_or_save()

if __name__ == "__main__":
    res.help_msg("\n[SPECIFIC OPTIONS]")
    todo = res.opt_string("-todo", "Taint;WCSC;QC;S&S;S&SFB", desc = "Select algorithms for drawing domains (default: Taint;WCSC;QC;S&S;S&SFB).").split(";")
    nolabels = res.opt_enable("-no-labels", "Disable y axis labels.")
    res.help_msg("\nPlot domain information for various algorithms.")
    res.options_done()
    mk_plot(res.load(), todo, nolabels)
