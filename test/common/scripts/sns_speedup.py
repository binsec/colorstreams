import results as res
import cactus
import numpy as np

def mean(l, trim = 0):
    lo = int(np.ceil(len(l) * trim))
    hi = int(np.floor(len(l) * (1 - trim)))
    return np.average(np.sort(np.array(l))[lo:hi])

def get_speedups(data, sink_filter = res.sfilter, sns_splits = res.sns_splits, trim = 0.05):
    entries = cactus.flatten(res.get_runtimes(data, ["Newsome", "S&S", "S&SFB"], sink_filter = sink_filter, sns_splits = sns_splits))
    runtimes = {"Newsome": []}
    algos = ["Newsome"]
    for entry in entries:
        sns = []
        for sns in (k for k in entry.keys() if k.startswith("S&S") and not k in algos):
            algos.append(sns)
            runtimes[sns] = [None for i in range(len(runtimes["Newsome"]))]
        for algo in algos:
            if algo in entry.keys():
                runtimes[algo].append(entry[algo])
            else:
                runtimes[algo].append(None)
    avgs = {}
    for algo, times in runtimes.items():
        times = [t for t in times if t]
        avgs[algo] = mean(times), mean(times, trim)
    spds = {}
    avg_spds = {}
    for algo, times in runtimes.items():
        if algo != "Newsome":
            spds[algo] = [x / y for x, y in zip(runtimes["Newsome"], times) if x and y]
            avg_spds[algo] = mean(spds[algo]), mean(spds[algo], trim)
    return avgs, spds, avg_spds

if __name__ == "__main__":
    res.help_msg("\n[SPECIFIC OPTIONS]")
    trim = float(res.opt_string("-trim", "0.05", "Set average trim (default: 0.05)."))
    res.help_msg("\nDisplay trimmed runtime averages and speedup averages for S&S over Newsome.")
    res.options_done()
    avgs, _, avg_spds = get_speedups(res.load(), trim = trim)
    print("Runtime averages (%d%% trimmed):" % (trim * 100))
    for algo, (avg, trimmed) in avgs.items():
        print("    %s: %.3g (%.3g)" % (algo, avg, trimmed))
    print("\nSpeedup averages (%d%% trimmed):" % (trim * 100))
    for algo, (spd, trimmed) in avg_spds.items():
        print("    %s: %.3g (%.3g)" % (algo, spd, trimmed))
