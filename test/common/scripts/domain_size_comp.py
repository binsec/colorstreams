import results as res
import re
import domains
import tabulate

def mk_comp(sns, tag1, tag2):
    data1 = res.get_basic_results(res.load(tfilter = re.compile(tag1)), toget = [sns])
    data2 = res.get_basic_results(res.load(tfilter = re.compile(tag2)), toget = [sns])
    get_prog = lambda bug: bug.split(".")[0]
    entries = []
    for entry1 in data1:
        entry = {}
        prog = get_prog(entry1["Bug"])
        matches = [entry for entry in data2 if entry["Bug"].startswith(prog) and entry["Sink"] == entry1["Sink"]]
        if matches == []:
            continue
        entry2 = matches[0]
        entry["Bug"] = prog
        entry["Sink"] = entry1["Sink"]
        entry["Domain Width Ratio"] = {}
        for snsk, counts in entry1[sns].items():
            if snsk in entry2[sns].keys():
                _, hi1, _ = entry1[sns][snsk]
                _, hi2, _ = entry2[sns][snsk]
                entry["Domain Width Ratio"][snsk] = hi2 / hi1
        entries.append(entry)
    return entries

if __name__ == "__main__":
    res.help_msg("\n[SPECIFIC OPTIONS]")
    sns = res.opt_string("-sns", "S&SFB", desc = "Select S&S algorithm.")
    tag1 = res.opt_regex_filter("-tag1", "$^", "default: empty")
    tag2 = res.opt_regex_filter("-tag2", "$^", "default: empty")
    res.help_msg("\nCompare domain width ratios between different analyses.\nUse -tag1 and -tag2 to specify which analyses must be compared.")
    res.options_done()
    entries = mk_comp(sns, tag1, tag2)
    table = []
    headers = ["Bug", "Sink"]
    headers_ = []
    for entry in entries:
        for snsk, _ in entry["Domain Width Ratio"].items():
            if not snsk in headers_:
                headers_.append(snsk)
    for snsk in headers_:
        headers.append(sns + " (" + snsk + ")")
    for entry in entries:
        tabentry = []
        tabentry.append(entry["Bug"])
        tabentry.append(entry["Sink"])
        for snsk in headers_:
            if snsk in entry["Domain Width Ratio"].keys():
                tabentry.append(entry["Domain Width Ratio"][snsk])
            else:
                tabentry.append(None)
        table.append(tabentry)
    print(tabulate.tabulate(table, headers = headers, tablefmt = res.fmt, floatfmt = ".3g"))
