# Results Exploitation Scripts

We provide the following scripts:

- `result.py`: display raw and comparative results under various forms
- `cactus.py`: display analysis runtimes as a cactus plot
- `domains.py`: display domains of control information for various algorithms
- `oob_scores.py`: display computed OOB capability scores
- `cfh_scores.py`: display computed CFH capability scores
- `sns_speedups.py`: display S&S speedup over Newsome
- `symbolic_stats.py`: display symbolic execution statistics
- `sns_counts.py`: evaluate the density of non-strong S&S intervals based on counting

Available options for each script can be displayed with the `-help` option.

All scripts assume that results are located in an *out* folder in the working directory.
Other output folders can be specified with the `-out` option.
Multiple can be selected by separating them with ';'.

*tip:* the `symbolic_stats.py` script can be used to monitor the progress of benchmark analyses in the following way:

```bash
watch -n <seconds between refreshes> python3 symbolic_stats.py
```
