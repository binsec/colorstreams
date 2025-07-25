# Ground-Truth Benchmark

## Running analyses

First, build the target programs:
```bash
make build
```

Then run all analyses:
```bash
make
```

To run the basic analyses:
```bash
make full
```

To run the control-flow hijack analyses:
```bash
make cfh
```

To run the S&S with counting analyses:
```bash
make snscount
```

To run the relaxation analyses:
```bash
make relax
```

To run the input concretization analyses:
```bash
make concr
```

You can run single analyses with `make <directory>/<vulnerability name>[.<analysis name>]` (e.g., `make minesweeper/minesweeper2` or `make sdop/cve-2024-41881.cfh`).

The following variables allow to customize analysis parameters:
- `CONFIG_OVERRIDE`: override the base configuration file (for example, with `config/light.config` to only run S&S with 100 maximum splits)
- `OUT`: specify an output folder other than `out`
- `VERBOSE`: controls the verbosity level
- `LS`: print stats to `stats.log` every specified number of analyzed instructions
- `FILTER_OUT`: exclude some experiments (wildcard supported)

## Configuration setup

Each analysis is configured through a *global* configuration file in `config` (`default.config` for basic analyses and `<name>.config` for named ones) and *target-specific* ones in `<target name>/config` (`<vulnerability name>.config` for basic analyses and `<vulnerability name>.<analysis name>.config` for named ones).

## Adding target programs / vulnerabilities

Each target must have its own directory, with at least a `makefile` and a `config` directory.

The `makefile` should append the vulnerabilities to the `ALL` variable (for building) and to other relevant variables (see `./makefile`).
It can also specify particular building rules for target programs (see `uboot/makefile`, default is `%.run` in the main `makefile`) and setup commands to be run before analysis (see `heartbleed/makefile`).

If a program must be run inside a nix shell, add a `xxx.nix` rule to the makefile and a `shell.nix` file (see `perl-dbi/makefile` and `perl-dbi/shell.nix`).

The `config` directory must contain configuration files for each vulnerability, named `<vulnerability name>.config` (or `<vulnerability name>.<analysis name>.config` for named analyses such as `cfh`).

Target programs must be manually instrumented and linked against `libpinstrio.a` (`pinstrio_stubs` nix package).
Building targets through *nix* is highly recommended to ensure build reproducibility (see `heartbleed/default.nix`).
