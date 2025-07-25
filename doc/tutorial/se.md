# Symbolic Execution Tutorial

## Objectives

This tutorial will teach you the basics of symbolic execution with *colorstreams*, in particular:
- specificities of *colorstreams* compared to traditional SE engines
- generating path constraints as SMT formulas
- general tips to improve scalability
- checking properties

## General concepts

*Colorstreams* offers symbolic execution capabilities based on the binary-level SE engine *binsec*.
SE usually consists in exploring a program while building a mathematical representation of constraints on variables, memory locations etc...
In particular, most SE engines explore all feasible execution paths, splitting path constraints on branching instructions while culling those which are no longer feasible.
While this approach enables extremely precise analysis, scalability is an issue as the number of paths to explore increases exponentially with each encountered branching instruction.
In addition, constraints can become too complex to solve for state-of-the-art SMT solvers.

As *colorstreams*' purpose is to analyze already discovered program execution traces, SE in *colorstreams* forgoes path exploration to follow a single concrete program execution.
This approach allows for much greater scalability and gives some guarantees on the validity of results, as *colorstreams* can detect when symbolic and concrete program states diverge and issue warnings and corrections.

The main limitation of SE with *colorstreams* is that the resulting constraints may be tighter than desired, as multi-path behaviour cannot be accounted for.
For example, the constraints on an integer parsed from a string with *atoi* may only include a subset of possibilities due to character-dependent control-flow.
One way to mitigate this issue is to update symbolic states based on hand-crafted models for comon library functions.
*Colorstreams*' *symbolic* analysis policy implements this through *stubs*, callbacks which are specific to some functions and that can be configured from options, although few are currently implemented.

### Definitions

- *symbolic state*:
The current path constraint associated with the analyzed program execution.
*Colorstreams* outputs symbolic states as SMT formulas.

- *free variable*:
We call *free variable* an unconstrained variable in a symbolic state, used to represent user or environmental inputs.
If path constraints can be seen as mathematical functions, free variables are their arguments.

- *symbolic* byte / variable: 
We call a byte of memory or variable *symbolic* if it is not constrained to a single value, i.e., it depends on a free variable and more than one value is feasible.

- *concrete* byte / variable:
We call a byte of memory or variable *concrete* if it is constrained to a single value.
To *concretize* a symbolic variable means to assign a *concrete* value to it, usually the one from the concrete program execution state.

## Generating path constraints with *colorstreams*

Let us now walk through how to generate path constraints for the vulnerabilities of the *target* program.
In addition to the options described in the [quickstart tutorial](./quickstart.md), the *symbolic* policy also defines its own, prefixed with `-symbolic`.
In particular, we will use `-symbolic-cw` to specify functions to be executed concretely, i.e., all writes performed during their execution are concretized.
This is useful to effectively ignore irrelevant functions, such as IO and memory allocation primitives.

By default, *colorstreams* saves file outputs to `/tmp/colorstreams-program-<timestamp>`.
Temporary files can be redirected to any folder using the `-keep-tmp` option.

To compute the path constraints for vulnerability (a) and save formulas to `out_a`, run the following command:
```bash
colorstreams -main main -keep-tmp out_a -p symbolic -symbolic-cw "_IO_printf;_IO_puts" -args "\"(a\" 0" ./target.run
```

You should obtain the following output:
```
[COLORSTREAMS] Chosen policies: symbolic
[Symbolic Policy <0> → SOURCE] message <0> <MEM<0x7fff02af67de>[2]>: controlled (custom)
[Symbolic Policy <0> → SOURCE] size <1> <MEM<0x7fff02af5210>[8]>: controlled (custom)
[Symbolic Policy <0> → SINK] size <0> <MEM<0x7fff02af50e8>[8]> (custom)
[Symbolic Policy <0> → RESULT]
size <0>: 
├─Sink: MEM<0x7fff02af50e8>[8]
└─Symbolic: <True, True, True, True, True, True, True, True>
  ├─Symbolic execution runtime: 0.065s
  └─Tracer runtime: 2.074s
[Symbolic Policy <0> → SINK] of_wsize <1> <MEM<0x7fff02af50d8>[8]> (custom)
[Symbolic Policy <0> → RESULT]
of_wsize <1>: 
├─Sink: MEM<0x7fff02af50d8>[8]
├─Symbolic: <True, True, True, True, True, True, True, True>
│ ├─Symbolic execution runtime: 0.065s
│ └─Tracer runtime: 2.075s
└─Constraint: sat
[Symbolic Policy <0> → STATS]
Symbolic Policy <0> stats:
├─Preprocessed sat: 110
├─Preprocessed unsat: 2
├─Preprocessed const: 0
├─Solver sat: 99
├─Solver unsat: 15
├─Solver error: 0
├─Solver runtime: 0.070s
├─Symbolic execution runtime: 0.065s
├─Symbolic instructions: 25
├─Concrete instructions: 3561
├─Processed function calls: 77
├─Processed instructions: 3586
├─Unique processed instructions: 1629
├─Sources: 2
├─Sinks: 2
├─Positive sinks: 2
└─Analysis runtime: 0.073s
[STATS]
General stats:
├─Warnings: 0
└─Total runtime: 2.303s
[DONE] 
```

`out_a` contains the following files:
```bash
> ls out_a
of_wsize_1_general_state.smt2  of_wsize_1_simplified.smt2  size_0_processed.smt2
of_wsize_1_processed.smt2      size_0_general_state.smt2   size_0_simplified.smt2
```

For both sinks, we have the following formulas:
- *general state*: raw symbolic state SMT formula
- *simplified*: simplified formula for a specific sink
- *processed*: different representation of simplified formulas with functions parameterized by free variables, allowing to reason over multiple traces

Similarly, for vulnerability (b):
```bash
colorstreams -main main -keep-tmp out_b -p symbolic -symbolic-cw "_IO_printf;_IO_puts" -args "\"(a\" 5000" ./target.run
```

You should obtain the following output:
```
[COLORSTREAMS] Chosen policies: symbolic
[Symbolic Policy <0> → SOURCE] message <0> <MEM<0x7ffe5c9797db>[2]>: controlled (custom)
[Symbolic Policy <0> → SOURCE] size <1> <MEM<0x7ffe5c9773d0>[8]>: controlled (custom)
[Symbolic Policy <0> → SINK] size <0> <MEM<0x7ffe5c9772a8>[8]> (custom)
[Symbolic Policy <0> → RESULT]
size <0>: 
├─Sink: MEM<0x7ffe5c9772a8>[8]
└─Symbolic: <True, True, False, False, False, False, False, False>
  ├─Symbolic execution runtime: 0.048s
  └─Tracer runtime: 2.071s
[Symbolic Policy <0> → SINK] of_wsize <1> <MEM<0x7ffe5c977298>[8]> (custom)
[Symbolic Policy <0> → RESULT]
of_wsize <1>: 
├─Sink: MEM<0x7ffe5c977298>[8]
├─Symbolic: <True, True, True, True, True, True, True, True>
│ ├─Symbolic execution runtime: 0.048s
│ └─Tracer runtime: 2.073s
└─Constraint: sat
[Symbolic Policy <0> → STATS]
Symbolic Policy <0> stats:
├─Preprocessed sat: 108
├─Preprocessed unsat: 1
├─Preprocessed const: 0
├─Solver sat: 73
├─Solver unsat: 32
├─Solver error: 0
├─Solver runtime: 0.051s
├─Symbolic execution runtime: 0.048s
├─Symbolic instructions: 22
├─Concrete instructions: 3390
├─Processed function calls: 77
├─Processed instructions: 3412
├─Unique processed instructions: 1628
├─Sources: 2
├─Sinks: 2
├─Positive sinks: 2
└─Analysis runtime: 0.057s
[STATS]
General stats:
├─Warnings: 0
└─Total runtime: 2.279s
[DONE]
``` 

## "Processed" SMT formulas

Let us now have a closer look at the "processed" SMT formulas generated by *colorstreams*, in particular `size_0_processed.smt2` for vulnerability (a):
```
(define-fun !8 ((size_1!2 (_ BitVec 64)) (message_0!1 (_ BitVec 16)))
  (_ BitVec 64) (bvsub (_ bv296 64) size_1!2))
(define-fun !b ((size_1!2 (_ BitVec 64)) (message_0!1 (_ BitVec 16)))
  (_ BitVec 8) ((_ extract 7 0) message_0!1))
(define-fun !e ((size_1!2 (_ BitVec 64)) (message_0!1 (_ BitVec 16)))
  (_ BitVec 56) 
  (bvand
  ((_ extract 63 8)
  (concat (_ bv91578483879936 48) 
  (concat ((_ extract 15 8) message_0!1) (!b size_1!2 message_0!1))))
  (_ bv15 56)))
(define-fun !d ((size_1!2 (_ BitVec 64)) (message_0!1 (_ BitVec 16)))
  (_ BitVec 64) (concat (!e size_1!2 message_0!1) (!b size_1!2 message_0!1)))
(define-fun size_0_proj_0 ((size_1!2 (_ BitVec 64))
  (message_0!1 (_ BitVec 16))) (_ BitVec 64) (bvsub size_1!2 (_ bv40 64)))
(define-fun additional_constraints ((size_1!2 (_ BitVec 64))
  (message_0!1 (_ BitVec 16))) Bool true)
(define-fun path_constraint ((size_1!2 (_ BitVec 64))
  (message_0!1 (_ BitVec 16))) Bool
  (and
  (and
  (distinct
  (bvand
  (bvor
  (bvand (bvor size_1!2 (_ bv18446744073709551319 64))
  (!8 size_1!2 message_0!1))
  (bvand (bvand size_1!2 (bvnot (!8 size_1!2 message_0!1)))
  (_ bv18446744073709551319 64))) (_ bv9223372036854775808 64))
  (_ bv9223372036854775808 64))
  (or 
  (=  
  (bvand
  (bvand (bvsub (!d size_1!2 message_0!1) (_ bv296 64))
  (bvnot (!d size_1!2 message_0!1))) (_ bv9223372036854775808 64))
  (_ bv9223372036854775808 64))
  (and (= (!e size_1!2 message_0!1) (_ bv1 56))
  (= (!b size_1!2 message_0!1) (_ bv40 8)))))
  (additional_constraints size_1!2 message_0!1)))
```

This formula is incomplete, as it only contains function definitions.
The purpose of these *processed* formulas is to serve as a base to express more complex properties, which may require reasoning over multiple traces.
To that end, they always provide the following basic building blocks:
- a *path_constraint* function, which takes free variables (inputs) as arguments and applies the corresponding execution path's constraints to them
- an *additional_constraints* function, which can be used to add custom constraints the path constraint
- a *xxx_proj_[0-9]+* function, which expresses the value of a projection, i.e. target variable / sink

As an example, checking that the path constraint is satisfiable and extracting a feasible value for the *size* sink can be done by adding the following lines to the end of the formula:
```
(declare-fun size () (_ BitVec 64))
(declare-fun message () (_ BitVec 16))
(declare-fun final_size () (_ BitVec 64))
(assert (path_constraint size message))
(assert (= (size_0_proj_0 size message) final_size))
(check-sat)
(get-model)
```

The *final_size* free variable is constrained to the value of the *size* sink and is part of the formula's satifying model.

To showcase how these formulas can be used to verify multi-trace properties, here is a way to check for *weak control* (i.e., more than one value is feasible):
```
(declare-fun size1 () (_ BitVec 64))
(declare-fun message1 () (_ BitVec 16))
(assert (path_constraint size1 message1))
(declare-fun size2 () (_ BitVec 64))
(declare-fun message2 () (_ BitVec 16))
(assert (path_constraint size2 message2))
(assert (distinct (size_0_proj_0 size1 message1) (size_0_proj_0 size2 message2)))
(check-sat)
```

## Tips and tricks

- As already stated, specifying irrelevant functions to be concretely executed can be useful to improve performance.
It is generally a good idea to start with IO primities such as *printf* and *puts*, as well as memory allocation primitives such as *malloc* and *free*.
The exact names of these functions may vary depending on the analyzed binaries.
Hard to analyze functions such as cryptographic primitives should also be considered.
Note that the `symbolic-cw` option can also accept regular expressions, specified as `<expr>`.

- Reducing the number of source symbolic bytes to a strict minimum can greatly improve performance.
A good approach is to first run taint analysis with the *bytedep* policy and only select the source bytes shown to impact sinks.
This can be done using the `-symbolic-src-bytes` option.

- Symbolic read / write addresses are another source of scalability issues, as the entire memory may be targeted in some cases.
By default, *Colorstreams* restricts symbolic addresses to a window around the concrete value (16 bytes in each direction, can be changed or disabled with the `-symbolic-saww` option).
They can also be concretized systematically with the `-symbolic-crwa` option (equivalent to `-symbolic-saww 0`).
Note that these approaches do not impact global constraints.
Thus, diverging data-flows depending on the same inputs are not affected, which may induce over-approximation.
On the other hand, unrestricted symbolic memory accesses may introduce unwanted symbolic bytes as Binsec treats unknown memory as unconstrained.

## Checking properties

*Colorstreams* implements the verification of advanced properties on SMT formulas.
The list of available ones can be displayed with the following command:
```bash
colorstreams -list-symbolic-properties
```

Properties to be verified by the *symbolic* policy can then be selected with the `-symbolic-p` option.

We will now go through a few examples.

### The *Count* property

*Count* performs projected model counting, yieding the number of feasible values for a sink.

Vulnerability (a):
```
[Symbolic Policy <0> → SINK] size <0> <MEM<0x7fff3d94f9f8>[8]> (custom)
Symbolic Policy <0> → RESULT]
size <0>: 
├─Sink: MEM<0x7fff3d94f9f8>[8]
├─Symbolic: <True, True, True, True, True, True, True, True>
│ ├─Symbolic execution runtime: 0.066s
│ └─Tracer runtime: 2.150s
└─Count: 
  ├─projection: size_0_proj_0
  └─result: 
    └─d4: 0x129
      └─Runtime: 0.107s
[Symbolic Policy <0> → SINK] of_wsize <1> <MEM<0x7fff3d94f9e8>[8]> (custom)
[Symbolic Policy <0> → RESULT]
of_wsize <1>: 
├─Sink: MEM<0x7fff3d94f9e8>[8]
├─Symbolic: <True, True, True, True, True, True, True, True>
│ ├─Symbolic execution runtime: 0.066s
│ └─Tracer runtime: 2.152s
├─Constraint: sat
└─Count: 
  ├─projection: of_wsize_1_proj_0
  └─result: 
    └─d4: 0x28
      └─Runtime: 0.090s
```

Vulnerability (b):
```
[Symbolic Policy <0> → SINK] size <0> <MEM<0x7ffd2928d948>[8]> (custom)
[Symbolic Policy <0> → RESULT]
size <0>: 
├─Sink: MEM<0x7ffd2928d948>[8]
├─Symbolic: <True, True, False, False, False, False, False, False>
│ ├─Symbolic execution runtime: 0.048s
│ └─Tracer runtime: 2.125s
└─Count: 
  ├─projection: size_0_proj_0
  └─result: 
    └─d4: 0x129
      └─Runtime: 0.074s
[Symbolic Policy <0> → SINK] of_wsize <1> <MEM<0x7ffd2928d938>[8]> (custom)
[Symbolic Policy <0> → RESULT]
of_wsize <1>: 
├─Sink: MEM<0x7ffd2928d938>[8]
├─Symbolic: <True, True, True, True, True, True, True, True>
│ ├─Symbolic execution runtime: 0.048s
│ └─Tracer runtime: 2.128s
├─Constraint: sat
└─Count: 
  ├─projection: of_wsize_1_proj_0
  └─result: 
    └─d4: 0x28
      └─Runtime: 0.079s
```

Relevant options:
- `-list-pmc-solvers`: list available projected model counting solvers
- `-pmcs`: select PMC solvers (separated with ';')
- `-pmct`: sets a runtime limit for PMC solvers (milliseconds)

## The *S&S* property

*S&S* computes *domains of control*, i.e., the set of feasible values for a sink.
*Weak control* means that at least two values are feasible, while *strong control* refers to all possible values being feasible.

Vulnerability (a):
```
[Symbolic Policy <0> → SINK] size <0> <MEM<0x7fffcecfe068>[8]> (custom)
[Symbolic Policy <0> → RESULT]
size <0>: 
├─Sink: MEM<0x7fffcecfe068>[8]
├─Symbolic: <True, True, True, True, True, True, True, True>
│ ├─Symbolic execution runtime: 0.066s
│ └─Tracer runtime: 2.136s
└─S&S: 
  ├─projection: size_0_proj_0
  └─result: 
    └─S&S: 
      └─no split limit: 
        ├─I0: 
        │ ├─interval: [0x0; 0x100] (0x101)
        │ └─status: strong
        ├─I1: 
        │ ├─interval: [0xffffffffffffffd8; 0xffffffffffffffff] (0x28)
        │ └─status: strong
        ├─Count: 
        │ ├─min: 0x129
        │ ├─maybe min: 0x129
        │ ├─max: 0x129
        │ └─pmc fallback: False
        ├─Splits: 1
        ├─Maximum Splits: -1
        ├─Runtime: 0.124s
        └─Timeout: false
[Symbolic Policy <0> → SINK] of_wsize <1> <MEM<0x7fffcecfe058>[8]> (custom)
[Symbolic Policy <0> → RESULT]
of_wsize <1>: 
├─Sink: MEM<0x7fffcecfe058>[8]
├─Symbolic: <True, True, True, True, True, True, True, True>
│ ├─Symbolic execution runtime: 0.066s
│ └─Tracer runtime: 2.138s
├─Constraint: sat
└─S&S: 
  ├─projection: of_wsize_1_proj_0
  └─result: 
    └─S&S: 
      └─no split limit: 
        ├─I0: 
        │ ├─interval: [0xfffffffffffffed8; 0xfffffffffffffeff] (0x28)
        │ └─status: strong
        ├─Count: 
        │ ├─min: 0x28
        │ ├─maybe min: 0x28
        │ ├─max: 0x28
        │ └─pmc fallback: False
        ├─Splits: 0
        ├─Maximum Splits: -1
        ├─Runtime: 0.049s
        └─Timeout: false
```

Vulnerability (b):
```
[Symbolic Policy <0> → SINK] size <0> <MEM<0x7ffd57090cd8>[8]> (custom)
[Symbolic Policy <0> → RESULT]
size <0>: 
├─Sink: MEM<0x7ffd57090cd8>[8]
├─Symbolic: <True, True, False, False, False, False, False, False>
│ ├─Symbolic execution runtime: 0.049s
│ └─Tracer runtime: 2.173s
└─S&S: 
  ├─projection: size_0_proj_0
  └─result: 
    └─S&S: 
      └─no split limit: 
        ├─I0: 
        │ ├─interval: [0x0; 0x128] (0x129)
        │ └─status: strong
        ├─Count: 
        │ ├─min: 0x129
        │ ├─maybe min: 0x129
        │ ├─max: 0x129
        │ └─pmc fallback: False
        ├─Splits: 0
        ├─Maximum Splits: -1
        ├─Runtime: 0.085s
        └─Timeout: false
[Symbolic Policy <0> → SINK] of_wsize <1> <MEM<0x7ffd57090cc8>[8]> (custom)
[Symbolic Policy <0> → RESULT]
of_wsize <1>: 
├─Sink: MEM<0x7ffd57090cc8>[8]
├─Symbolic: <True, True, True, True, True, True, True, True>
│ ├─Symbolic execution runtime: 0.049s
│ └─Tracer runtime: 2.176s
├─Constraint: sat
└─S&S: 
  ├─projection: of_wsize_1_proj_0
  └─result: 
    └─S&S: 
      └─no split limit: 
        ├─I0: 
        │ ├─interval: [0x1; 0x28] (0x28)
        │ └─status: strong
        ├─Count: 
        │ ├─min: 0x28
        │ ├─maybe min: 0x28
        │ ├─max: 0x28
        │ └─pmc fallback: False
        ├─Splits: 0
        ├─Maximum Splits: -1
        ├─Runtime: 0.050s
        └─Timeout: false
```

Relevant options:
- `-sns-l`: limit the number of *splits*, i.e., the number of intervals
- `-sns-t`: limits the runtime of S&S (seconds)
- `-sns-pmc`: compute PMC counts for intervals where *strong control* cannot be proven, with the specified solver(s)
- `-sns-constr`: take additional regularity contraints into account
- `-sns-list-constrs`: list available regularity constraints
