import tabulate as tab
import results as res
import domains
import collections

avg = lambda l: sum(l) / len(l)
density = lambda itv: itv["card_hi"] / (itv["hi"] - itv["lo"] + 1)

def maybe_add(l, itv):
    if itv["card_hi"] == itv["card_lo"]:
        l.append(density(itv))

def maybe_avg(l):
    if l == []:
        return None
    else:
        return avg(l)

def get_densities(data, splits = res.sns_splits, sink_filter = res.sfilter):
    entries = collections.OrderedDict()
    def maybe_make_entry(entry):
        name = domains.mk_name(entry)
        if not name in entries.keys():
            entries[name] = {"Bug": entry["Bug"], "Sink": entry["Sink"], "Size": entry["Size"], "ItvDensities": {}}
        return name
    def get_sns(sns):
        for snsk, entries_ in domains.get_sns_from_symb(data, name = sns, splits = splits, sink_filter = sink_filter).items():
            for entry in entries_:
                name = maybe_make_entry(entry)
                if entry["Domain"]:
                    weak, maybe, all_ = [], [], []
                    t = 0
                    for itv in entry["Domain"]:
                        if itv["card_hi"] != itv["card_lo"]:
                            t += 1
                        elif itv["status"] == "weak":
                            maybe_add(weak, itv)
                            maybe_add(all_, itv)
                        elif itv["status"] == "maybe strong":
                            maybe_add(maybe, itv)
                            maybe_add(all_, itv)
                    densities = maybe_avg(weak), maybe_avg(maybe), maybe_avg(all_), t
                    entries[name]["ItvDensities"][sns + " (" + snsk + ")"] = densities
    get_sns("S&S")
    get_sns("S&SFB")
    return entries

def mk_table():
    def maybe_format(f):
        if f:
            return "%.3g" % f
        else:
            return "None"
    def format(e):
        weak, maybe, all_, t = e
        return maybe_format(weak), maybe_format(maybe), maybe_format(all_), t
    data = get_densities(res.load())
    table = []
    headers = ["Bug", "Sink", "Size"]
    overall = {}
    for _, entry in data.items():
        tabentry = []
        tabentry.append(entry["Bug"])
        tabentry.append(entry["Sink"])
        tabentry.append(entry["Size"] * 8)
        for k, densities in entry["ItvDensities"].items():
            if not k in headers:
                headers.append(k)
            if not k in overall.keys():
                overall[k] = []
            tabentry.append(format(densities))
            overall[k].append(densities)
        table.append(tabentry)
    lastentry = ["Overall", "", ""]
    for _, l in overall.items():
        weak = [weak for weak, _, _, _ in l if weak]
        maybe = [maybe for _, maybe, _, _ in l if maybe]
        all_ = [all_ for _, _, all_, _ in l if all_]
        t = sum([t for _, _, _, t in l])
        lastentry.append((maybe_format(maybe_avg(weak)), maybe_format(maybe_avg(maybe)), maybe_format(maybe_avg(all_)), t))
    table.append(lastentry)
    table = tab.tabulate(table, headers = headers, floatfmt = ".3g", tablefmt = res.fmt)
    print("Interval densities for weak intervals (weak, maybe_strong, both, timeouts):")
    print()
    print(table)


if __name__ == "__main__":
    res.help_msg("\nAnalysis of weak interval densities for S&S based on counting (for analyses with -sns-pmc enabled).")
    res.options_done()
    mk_table()
