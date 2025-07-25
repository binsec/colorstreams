# Pinstrio

Interactive instruction input / output tracing and program state inspection for x86 and x86_64.

## Building

- x86_64:
```
make PIN_ROOT=<path to the pin kit> obj-intel64/pinstrio.so
```
- x86
```
make PIN_ROOT=<path to the pin kit> obj-ia32/pinstrio.so TARGET=ia32
```

## Usage

Command line:
```
pin [pin options] -t <path>/pinstrio.so [pinstrio options] -- <target binary>
```

Available options:
```
-atf  [default 0]
	Always trace functions.
-binary  [default 0]
	Communicate instruction traces as binary data.
-in  [default ]
	Accept inputs through a named pipe instead of stdin.
-logfile  [default pintool.log]
	The log file path and file name
-main  [default ]
	Main function (default: trace everything).
-no_abort  [default 0]
	Ignore aborts.
-no_avx512  [default 0]
	Disable AVX.
-out  [default ]
	Communicate through a named pipe instead of stdout.
-start  [default 0]
	Immediately start tracing.
-unique_logfile  [default 0]
	The log file names will contain the pid
```

### Interaction and trace specification

Upon starting, Pinstrio outputs the message `Ready to start!`.
To proceed, simply press enter.

Pinstrio pauses execution between traces and waits for user commands, which can be the following (note that answers are always prefixed with `4`, the answer trace id):

* `q`: quit.
* `v REG<regname>|MEM<0xXXXXXXXX>[n]`: read the register `reg` or `n` bytes from the memory location at `0xXXXXXXXX`. Answers are formatted as hex bytes in little-endian. Returns `ERROR` if an error occurred. Not available on bbl traces.
* `p`: print the process' PID.
* `d`: trigger a breakpoint (only when synchronized with GDB, see the next section).
* `g 0xXXXXXXXX`: advance the execution until the instruction at the given address or the next trace point.
* `t`: print the template for the current instruction (only available when stopped on an instruction). Answers are formatted as `function name;address;opcode;pretty print;B|NB;P|NP;operands`:
    - `B|NB` denotes whether the instruction is or is not a control-flow instruction (branching / non-branching).
    - `P|NP` denotes whether the instruction is predicated or not (i.e., only executed based on some condition)
    - `operands` is a list of the instruction's operands, formatted as `num:R|W|RW<spec>` with `<spec>` one of `REG<reg:fullreg>[size:fullsize]` for registers, `MEM<0xXXXXXXXX>(segment+base+displacement+index*scale)[size]` for memory locations or `IMM<n>` for constants.

Traces start with a number character indicating their type.
If the `-binary` option is set, it can be used to determine how the associated binary data should be parsed.

Here is a list of all possible traces, their ids and their specification

* `0` Entering or exiting a function: `Entering|Exiting function_name`
* `0` Calling a function: `Call(id,value of rsp)`
* `0` After a call: `CallRet(id,value of rsp)`
* `1` Instructions (text mode): `address opcode NA|NB|B NA|NE|E first_read_address second_read_address write_address`. `NA|NB|B` indicates if the instruction is (in order) not a control-flow instruction, not branching or branching. `NA|NE|E` indicates whether the instruction is not predicated, predicated and not executed or predicated and executed. The addresses at the end are used to fill the corresponding template (`$0`, `$1` and `$2` respectively).
* `1` Instructions (binary mode): `address(8)opcode_size(8)opcode(variable)flags(1)read1(8)read2(8)write(8)` with flags such that:
    - bit 0: 1 if branching, else 0
    - bit 1: 1 if control-flow, else 0
    - bit 2: 1 if executed, else 0
    - bit 3: 1 if predicated, else 0
* `2` Sources (from code instrumentation, see below): `Source(address,size,tag)`
* `3` Sinks (from code instrumentation, see below): `Sink(address,size,name,value)` or `Sink(address,size,value,address_of_constraint,size_of_constraint,value_of_constraint)`
* `4` Answers (explained above)
* `5` Templates (explained above)
* `6` BBLs (basic blocks) (text mode): ` number_of_instructions address1:opcode1 ...`
* `6` BBLs (binary mode): `number_of_instructions(8)address1(8)opcode_size1(8)opcode1(variable)...`

### Code instrumentation

Programs can be manually instrumented with the stubs defined in `stubs/pinstrio.h`.
If needed, you can build a static library containing those stubs by running `make` in `stubs`.

Here is the list of available instrumentation stubs:
- `void _pinstrio_source_(void *ptr, int size, char *tag)`: trace a source
- `void _pinstrio_source_cnt_(void *ptr, int size, char *tag, int cnt)`: trace a source at most `cnt` times
- `void _pinstrio_sink_(void *ptr, int size, char *name)`: trace a sink
- `void _pinstrio_constrained_sink_(void *ptr, int size, char *name, void *constr, int csize)`: trace a constrained sink (`*constr` specifies some condition)
- `void _pinstrio_concrete_begin_()`: switch to concrete mode (for symbolic execution)
- `void _pinstrio_concrete_end_()`: exit concrete mode (for symbolic execution)
- `void _pinstrio_start_()`: start tracing
- `void _pinstrio_stop_()`: stop tracing
- `void _pinstrio_start_cnt_(int cnt)`: start tracing after `cnt` passes
- `void _pinstrio_abort_()`: stop the program

Pinstrio recognizes those functions and performs the associated tasks based on their arguments.
