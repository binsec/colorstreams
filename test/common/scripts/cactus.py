import matplotlib.pyplot as plt
import matplotlib.colors as clrs
import matplotlib.cm as cm
from matplotlib import markers
import results as res
import re
import numpy as np

pp = {
        "WeakControl": "WC",
        "NotStrongControl": "SC",
        "StrongControl" : "SC",
        "Count": "PMC",
        "Newsome": "Newsome",
        "S&S": "S&S",
        "S&SFB": "S&SFB",
        "SE": "SE",
        "Tracing": "Tracing"
    }

def flatten(runtimes):
    result = []
    for entry in runtimes:
        nentry = {}
        for k in entry.keys():
            if k == "Count":
                for pmc, runtime in entry[k].items():
                    nentry[pmc] = runtime
            elif k.startswith("S&S"):
                for splits, runtime in entry[k].items():
                    nentry[k + " (" + splits + ")"] = runtime
            else:
                nentry[k] = entry[k]
        result.append(nentry)
    return result

def lines_and_colors(runtimes):
    res = {}
    res["SE"] = ":", "grey"
    res["Tracing"] = ":", "lightgrey"
    res["WC"] = "-.", "blue"
    res["SC"] = "-.", "cyan"
    doms = []
    pmcs = []
    for entry in runtimes:
        for k, data in entry.items():
            if k == "Newsome" and not k in doms:
                doms.append(k)
            elif k.startswith("S&S"):
                for splits in entry[k].keys():
                    name = k + " (" + splits + ")"
                    if not name in doms:
                        doms.append(name)
            elif k == "Count":
                for pmc in entry[k].keys():
                    if not pmc in pmcs:
                        pmcs.append(pmc)
    doms_cnorm = clrs.Normalize(vmin = -2, vmax = len(doms))
    doms_colors = cm.ScalarMappable(norm = doms_cnorm, cmap = "YlOrRd")
    for dom, i in zip(doms, range(len(doms))):
        res[dom] = "-", doms_colors.to_rgba(i)
    pmcs_cnorm = clrs.Normalize(vmin = -2, vmax = len(pmcs))
    pmcs_colors = cm.ScalarMappable(norm = pmcs_cnorm, cmap = "RdPu")
    for pmc, i in zip(pmcs, range(len(pmcs))):
        res[pmc] = "--", pmcs_colors.to_rgba(i)
    return res

def mk_cactus(runtimes, lines_and_colors, print_cumul):
    data = {}
    for entry in runtimes:
        for k in entry.keys():
            value = entry[k]
            if isinstance(value, float):
                if k in data.keys():
                    data[k].append(value)
                else:
                    data[k] = [value]
    fig, ax = plt.subplots(dpi = res.dpi)
    maxlen = 0
    minnonz, maxx = 10000, 0
    for algo, runtimes in data.items():
        maxlen = max(maxlen, len(runtimes))
        runtimes.sort()
        if algo in pp.keys():
            label = pp[algo]
        else:
            label = algo
        if label in lines_and_colors.keys():
            line, color = lines_and_colors[label]
        else:
            line, color = None, None
        csum = [sum(runtimes[:(x + 1)]) for x in range(len(runtimes))]
        if print_cumul:
            print(algo, csum[-1])
        maxx = max(maxx, csum[-1])
        for t in csum:
            if t > 0:
                minnonz = min(minnonz, t)
                break
        ax.plot(range(1, len(runtimes) + 1), csum, label = label, linestyle = line, linewidth = 2, color = color)
    ax.set_xlabel("# of problems solved")
    major_itv = max(round(maxlen / 100), 1) * 10
    ax.set_xticks(range(0, maxlen + major_itv, major_itv))
    #ax.set_xticks(range(0, maxlen + 1, int(major_itv / 10)), minor = True)
    ax.set_ylabel("cumulative runtime (s)")
    ax.set_yscale("log")
    ax.set_yticks([10**exp for exp in range(int(np.floor(np.log10(minnonz))), int(np.ceil(np.log10(maxx))) + 1)])
    ax.legend(loc = res.legendloc, ncols = 2)
    if res.title:
        ax.set_title("Cactus Plot")
    fig.tight_layout()
    res.show_or_save()

if __name__ == "__main__":
    res.help_msg("\n[SPECIFIC OPTIONS]")
    todo = res.opt_string("-select-algos", "WeakControl;NotStrongControl;Count;Newsome;S&S;S&SFB;SE;Tracing", desc = "Select algorithms for the cactus plot (default: WeakControl;NotStrongControl;Count;Newsome;S&S;S&SFB;SE;Tracing).").split(";")
    print_cumul = res.opt_enable("-print-cumul", "Print cumulative runtimes.")
    res.help_msg("\nPlots cumulative runtime over solved instances.")
    res.options_done()
    data = res.get_runtimes(res.load(), todo)
    lac = lines_and_colors(data)
    mk_cactus(flatten(data), lac, print_cumul)
