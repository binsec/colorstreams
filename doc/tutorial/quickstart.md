# Quickstart Tutorial

## Objectives

This tutorial will teach you the basics of analyzing programs with *colorstreams*, in particular:
- general analysis concepts
- manual C source-level instrumentation
- basic command line setups
- taint analysis

By the end, you will be able to perform taint analysis on the provided *target* program.

## General analysis concepts

*Colorstreams* analyzes execution traces on-the-fly (inline) through callbacks defined in *analysis policies* and can thus inspect program states at any moment.
Tracing is performed by *colorstreams*' tracer, *pinstrio*, which is based on the Intel PIN framework.

*Colorstreams*' analysis policies are modelled after general information-flow analysis concepts.
In particular, they rely on data-flow initiators (e.g., user inputs) being marked as *sources*  and locations to check for dependency to them (e.g., an overwritten return address) as *sinks*.
For example, *taint analysis* consists in attributing taint tags to sources, then propagating them through executed instructions and checking whether they are found in sinks.

*Colorstreams* currently implements *taint-* and *symbolic-execution-based* data-flow analyses.
For the sake of simplicity, this tutorial focuses on taint analysis.
Follow the [symbolic execution tutorial](se.md) to learn how to use symbolic execution.

Analysis policies allow to create independent-yet-composable analyses, enabling a great degree of flexibility and automation.
Follow the [automation tutorial](auto.md) to learn more.

Use the following command to get a list of available analysis policies:
```
colorstreams -list-policies
```
## Target

*target* copies a user-provided message with a header, based on a user-provided size.
This program contains several vulnerabilities, in particular:
- (a) providing a size smaller than the message header (40 bytes) results in an integer underflow and a very large copy
- (b) providing a size greater than 256 results in the size of the copy being controlled by the message header

Below is *target*'s source code with comments further explaining the vulnerabilities.

```c
#include "stdio.h"
#include "stdint.h"
#include "string.h"

#define HEADER_SIZE 40

//checks that the message header is valid
uint64_t check_header(char *input, uint64_t input_size)
{
    //2) header sprayed on the stack
    uint64_t id = *((uint64_t *) input) & 0xfff;
    if(id <= 296)
    {
        uint64_t a = ((uint64_t *) input)[1];
        uint64_t b = ((uint64_t *) input)[2];
        //...
        return 1;
    }
    return 0;
}
    
//copies the message into a buffer
void get_msg(char *buf, uint64_t buf_size, 
        char *input, uint64_t input_size)
{
    //3) not initialized => size = sprayed header
    uint64_t size;
    if(input_size <= buf_size + HEADER_SIZE)
        //4) input_size < 40 => integer underflow
        size = input_size - HEADER_SIZE;
    //5) buffer overflow!!!
    // a. input_size < 40: 2^64 - 40 <= size < 2^64
    // b. input_size > 264: size = header 
    printf("copying %llu bytes into a %llu bytes buffer...\n", size, buf_size);
    memcpy(buf, input + HEADER_SIZE, size);
    printf("done\n");
}

//parses command line inputs
uint64_t get_input(int argc, char **argv, char **msg)
{
    if(argc != 3)
    {
        printf("Usage: target message message_size\n");
        exit(0);
    }
    *msg = argv[1];
    return atoi(argv[2]);
}

int main(int argc, char **argv)
{
    //1) inputs: char *input, uint64_t input_size
    char *input = NULL;
    uint64_t input_size = get_input(argc, argv, &input);
    char buf[256];
    if(check_header(input, input_size))
        get_msg(buf, 256, input, input_size);
    else
        printf("invalid message header\n");
    return 0;
}
```

## Manual C source-level instrumentation

The simplest way of setting up sources and sinks is to manually instrument the code using *pinstrio*'s API (*Colorstreams*'s tracer).
We will be particulary interested in the following instrumentation primitives:
- `_pinstrio_source_(void *buf, int size, char *desc)`: marks *buf[0 -> size-1]* as a source with the provided description
- `_pinstrio_sink_(void *buf, int size, char *name)`: marks *buf[0 -> size-1]* as a sink with the provided name
- `_pinstrio_constrained_sink_(void *buf, int size, char *name, void *constr, int csize)`: same as `_pinstrio_sink_` except *constr* and *csize* define a memory location containing a boolean, which is used to determine whether the sink is valid or not
- `_pinstrio_abort_()`: stops analysis

Source descriptions are formatted as `name:keyword1/keyword2/...`.
Some features rely on keywords to differentiate between different kinds of sources, as shown below.

Let us now instrument *target* for analyzing its vulnerabilities.

First, we need to mark *input* and *input_size* as sources:

```c
int main(int argc, char **argv)
{
    //1) inputs: char *input, uint64_t input_size
    char *input = NULL;
    uint64_t input_size = get_input(argc, argv, &input);
    _pinstrio_source_(input, strlen(input), "message:controlled");
    _pinstrio_source_(&input_size, 8, "size:controlled");
    char buf[256];
    if(check_header(input, input_size))
        get_msg(buf, 256, input, input_size);
    else
        printf("invalid message header\n");
    return 0;
}
```

The *controlled* keyword is recognized by the *basic* taint policy and marks user-controlled inputs.

We can then mark the copy's *size* as a sink and prevent wasting time in *memcpy* with `_pinstrio_abort_`, as *size* can be very large.
We can also filter out instances where there is no out-of-bounds write using `_pinstrio_constrained_sink_`.

```c
void get_msg(char *buf, uint64_t buf_size, 
        char *input, uint64_t input_size)
{
    //3) not initialized => size = sprayed header
    uint64_t size;
    if(input_size <= buf_size + HEADER_SIZE)
        //4) input_size < 40 => integer underflow
        size = input_size - HEADER_SIZE;
    //5) buffer overflow!!!
    // a. input_size < 40: 2^64 - 40 <= size < 2^64
    // b. input_size > 264: size = header 
    int oob = size > buf_size;
    uint64_t of_size = size - buf_size;
    printf("copying %llu bytes into a %llu bytes buffer...\n", size, buf_size);
    _pinstrio_sink_(&size, 8, "size");
    _pinstrio_constrained_sink_(&of_size, 8, "of_wsize", &oob, 4);
    _pinstrio_abort_();
    memcpy(buf, input + HEADER_SIZE, size);
    printf("done\n");
}
```

*target* is now ready to be analyzed.

## Performing taint analysis on *target*

First, compile *target* with `make`.
In general, programs should be compiled with debugging symbols as some analysis policies rely on GDB.

Some command line options are mandatory to use most analyses:
- `-main` selects the main function to be analyzed (*main* usually)
- `-p` selects top-level analysis policies (separated by ';')
- `-args` passes arguments to the target program

First, let us try the *basic* taint analysis policy.

Run the following command to analyze vulnerability (a):
```bash
colorstreams -main main -p basic -args "\"(a\" 0" ./target.run
```

You should obtain more or less the following output:
```
[COLORSTREAMS] Chosen policies: basic
[Basic Policy <0> → SOURCE] message <0> <MEM<0x7ffc6ece0804>[2]>: controlled (custom)
[Basic Policy <0> → SOURCE] size <1> <MEM<0x7ffc6ecdf080>[8]>: controlled (custom)
[Basic Policy <0> → SINK] size <0> <MEM<0x7ffc6ecdef58>[8]> (custom)
[Basic Policy <0> → RESULT]
size <0>: 
├─Sink: MEM<0x7ffc6ecdef58>[8]
└─Taint: 
  ├─MEM<0x7ffc6ecdef58>[8]: <C, C, C, C, C, C, C, C>
  ├─Tainted registers: 15
  └─Tainted memory bytes: 3728
[Basic Policy <0> → SINK] of_wsize <1> <MEM<0x7ffc6ecdef48>[8]> (custom)
[Basic Policy <0> → RESULT]
of_wsize <1>: 
├─Sink: MEM<0x7ffc6ecdef48>[8]
└─Taint: 
  ├─MEM<0x7ffc6ecdef48>[8]: <C, C, C, C, C, C, C, C>
  ├─Tainted registers: 14
  └─Tainted memory bytes: 3728
[Basic Policy <0> → STATS]
Basic Policy <0> stats:
├─Tainted registers: 14
├─Tainted memory bytes: 3728
├─Processed function calls: 77
├─Processed instructions: 3586
├─Unique processed instructions: 1629
├─Sources: 2
├─Sinks: 2
├─Positive sinks: 2
└─Analysis runtime: 0.003s
[STATS]
General stats:
├─Warnings: 0
└─Total runtime: 2.271s
[DONE] 
```

As you can see, the *message* and *size* sources appear in the output, as well as the *size* and *of_wsize* sinks.
As expected, both sinks are tainted.

Performing the same analysis on vulnerability (b)...
```bash
colorstreams -main main -p basic -args "\"(a\" 5000" ./target.run
```

...yields similar results:
```
[COLORSTREAMS] Chosen policies: basic
[Basic Policy <0> → SOURCE] message <0> <MEM<0x7ffe49c32801>[2]>: controlled (custom)
[Basic Policy <0> → SOURCE] size <1> <MEM<0x7ffe49c312a0>[8]>: controlled (custom)
[Basic Policy <0> → SINK] size <0> <MEM<0x7ffe49c31178>[8]> (custom)
[Basic Policy <0> → RESULT]
size <0>: 
├─Sink: MEM<0x7ffe49c31178>[8]
└─Taint: 
  ├─MEM<0x7ffe49c31178>[8]: <C, C, C, C, C, C, C, C>
  ├─Tainted registers: 15
  └─Tainted memory bytes: 3694
[Basic Policy <0> → SINK] of_wsize <1> <MEM<0x7ffe49c31168>[8]> (custom)
[Basic Policy <0> → RESULT]
of_wsize <1>: 
├─Sink: MEM<0x7ffe49c31168>[8]
└─Taint: 
  ├─MEM<0x7ffe49c31168>[8]: <C, C, C, C, C, C, C, C>
  ├─Tainted registers: 14
  └─Tainted memory bytes: 3694
[Basic Policy <0> → STATS]
Basic Policy <0> stats:
├─Tainted registers: 14
├─Tainted memory bytes: 3694
├─Processed function calls: 77
├─Processed instructions: 3412
├─Unique processed instructions: 1628
├─Sources: 2
├─Sinks: 2
├─Positive sinks: 2
└─Analysis runtime: 0.003s
[STATS]
General stats:
├─Warnings: 0
└─Total runtime: 2.286s
[DONE] 
```

But if we check a copy with no OOB write:
```bash
colorstreams -main main -p basic -args "\"(a\" 45" ./target.run
```

We see that *of_wsize*'s constraint is not met:
```
[COLORSTREAMS] Chosen policies: basic
[Basic Policy <0> → SOURCE] message <0> <MEM<0x7ffe241627dd>[2]>: controlled (custom)
[Basic Policy <0> → SOURCE] size <1> <MEM<0x7ffe24161720>[8]>: controlled (custom)
[Basic Policy <0> → SINK] size <0> <MEM<0x7ffe241615f8>[8]> (custom)
[Basic Policy <0> → RESULT]
size <0>: 
├─Sink: MEM<0x7ffe241615f8>[8]
└─Taint: 
  ├─MEM<0x7ffe241615f8>[8]: <C, C, C, C, C, C, C, C>
  ├─Tainted registers: 15
  └─Tainted memory bytes: 3690
[Basic Policy <0> → SINK] of_wsize <1> <MEM<0x7ffe241615e8>[8]> (custom)
[Basic Policy <0> → STATS]
Basic Policy <0> stats:
├─Tainted registers: 14
├─Tainted memory bytes: 3690
├─Processed function calls: 77
├─Processed instructions: 3346
├─Unique processed instructions: 1632
├─Sources: 2
├─Sinks: 2
├─Positive sinks: 1
└─Analysis runtime: 0.003s
[STATS]
General stats:
├─Warnings: 0
└─Total runtime: 2.311s
[DONE] 
```

We can also try the *bytedep* policy, which performs a more precise taint analysis allowing to track from which source bytes taint originates.
This way, we see that the *size* sink depends on bytes 0 to 7 from the *size* source for vulnerability (a):
```
[Byte Dependency Taint Policy <0> → RESULT]
size <0>: 
├─Sink: MEM<0x7ffeb74eead8>[8]
└─Taint: 
  ├─MEM<0x7ffeb74eead8>[8]: <D(size <1> <MEM<0x7ffeb74eec00>[8]>: controlled (custom))[0 - 7], D(size <1> <MEM<0x7ffeb74eec00>[8]>: controlled (custom))[0 - 7], D(size <1> <MEM<0x7ffeb74eec00>[8]>: controlled (custom))[0 - 7], D(size <1> <MEM<0x7ffeb74eec00>[8]>: controlled (custom))[0 - 7], D(size <1> <MEM<0x7ffeb74eec00>[8]>: controlled (custom))[0 - 7], D(size <1> <MEM<0x7ffeb74eec00>[8]>: controlled (custom))[0 - 7], D(size <1> <MEM<0x7ffeb74eec00>[8]>: controlled (custom))[0 - 7], D(size <1> <MEM<0x7ffeb74eec00>[8]>: controlled (custom))[0 - 7]>
  ├─Tainted registers: 1
  └─Tainted memory bytes: 50
```

But in the case of vulnerability (b), it depends the first bytes of *message*:
```
[Byte Dependency Taint Policy <0> → RESULT]
size <0>: 
├─Sink: MEM<0x7ffcfdd332b8>[8]
└─Taint: 
  ├─MEM<0x7ffcfdd332b8>[8]: <D(message <0> <MEM<0x7ffcfdd347db>[2]>: controlled (custom))[0], D(message <0> <MEM<0x7ffcfdd347db>[2]>: controlled (custom))[1], -, -, -, -, -, ->
  ├─Tainted registers: 1
  └─Tainted memory bytes: 38
```

## Configuration files

As command lines can become quite complex with more advanced analysis policies, *colorstreams* can read options from configuration files.
An example can be seen in `quickstart.conf`.

The target program is specified with the special *target* label.
Configuration files also support comments, started with '#'.

To run *colorstreams* with a configuration file (or several), simply run:
```bash
colorstreams -config <file1;file2;...>
```

Note that configuration files can themselves include others with the same "config" option.
Furthermore, options can be overridden by re-setting them later in the command line.
