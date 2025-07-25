# MAGMA Benchmark

## Reproducers

See [this page](repros/readme.md) for more information on each vulnerability and how they were selected.

## Running analyses

First, build the target programs:

```bash
make build
```

Then run the full analysis of the benchmark:
```bash
make
```

To run the byte dependency analysis:
```bash
make bytedep
```

To run the S&S with counting analysis:
```bash
make snscount
```

You can run single analyses with `make <vulnerability name>[.<analysis name>]` (e.g., `make TIF002` or `make TIF002.bytedep`).

The following variables allow to customize analysis parameters:
- `CONFIG_OVERRIDE`: override the base configuration file (for example, with `config/light.config` to only run S&S with 100 maximum splits)
- `OUT`: specify an output folder other than `out`
- `VERBOSE`: controls the verbosity level
- `LS`: print stats to `stats.log` every specified number of analyzed instructions

## Configuration setup

Each analysis is configured through various configuration files in `config`:
- *global* configuration: `default.config` for the full analysis and `<name>.config` for named ones (e.g., *bytedep*)
- *target-specific* configuration: `<program name>.config`
- *vulnerability-specific* configuration: `<vulnerability name>.config` for the full analysis and `<vulnerability name>.<analysis name>.config` for named ones

## Adding vulnerabilities

To add vulnerabilities, add the reproducing input in `repros`, update the `ALL` variable in `makefile` and provide adequate configuration in `config`. 

## Adding target programs

To add a new target program, package it through nix and add it to `default.nix`.
You should also write a configuration file for it in `config`, which can be included in vulnerability configuration files.

Use `fuzzer_main` to adapt libfuzzer-like fuzzers (see `../common/fuzzer_main.c` and `nix/libxml2.nix`).
