Magma fuzzing benchmark official website: https://hexhive.epfl.ch/magma/docs/bugs.html
Proof-of-concept inputs (pocs) from the magma paper evaluation: https://osf.io/resj8/

Only memory / pointer corruption bugs triggered during the original magma evaluation are tested. Pocs can also be found from bug reports online, however many are not openly available. 

Many bugs were reached but not triggered by fuzzers in the magma paper experiments. To find which pocs actually trigger bugs, use *check_pocs.sh* as follows:
```
./check_pocs.sh <program with asan> <poc name pattern>
```
Example:
```
./check_pocs.sh result/bin/libtiff_tiff_read_rgba_fuzzer_asan.run <path to moptafl pocs>/moptafl_libtiff_tiff_read_rgba_fuzzer_*
```
The resulting outputs are stored in the *out* directory. To easily find which pocs trigger bugs, use the following command:
```
grep -rn out -l -e "AddressSanitizer"
```

A same bug can be triggered through many different paths, with different caracteristics. It most likely would not be feasible to explore all of them in most real-world cases. We included alternate pocs when we observed different capabilities.

When multiple OOB reads and / or writes are observed, we picked the first one of each for analysis. For OOB writes, only the first 8 bytes of written data at most are analyzed.

# libxml2

challenge: string parsing

| Bug    | Fuzzer             | Type                        | Taint | Control |
| :--    | :-----             | :--:                        | :---- | :------ |
| XML001 | xmllint            | stack OOB write (size 4925) | data  | data    |
| XML002 | read_memory_fuzzer | heap OOB write (size 4)     | no    | no      |
| XML006 | xmllint            | stack OOB write (size 2)    | no    | no      |
| XML012 | read_memory_fuzzer | UAF reads (size 1)          | no    | no      |

Comments:
- XML001: size could be controlled through string length
- XML012: could be inside a loop with controlled bound

# poppler

challenge: floats

| Bug    | Fuzzer     | Type                             | Taint | Control |
| :--    | :-----     | :--:                             | :---- | :------ |
| PDF003 | pdfimages  | 2x stack OOB read (size 1)       | no    | no      |
| PDF004 | pdf_fuzzer | read at invalid address (size 8) | base  | no?     |
| PDF010 | pdf_fuzzer | read at invalid address (size 1) | no    | no      |
| PDF018 | pdf_fuzzer | read at invalid address (size 8) | no    | no      |
| PDF019 | pdf_fuzzer | heap OOB read (size 1)           | base  | base    |

Comments:
- PDF004: loss of symbolic bytes due to float conversions

# openssl

challenge: crypto

## ground truth bugs 

| Bug    | Fuzzer | Type                        | Taint      | Control    |
| :--    | :----- | :--:                        | :----      | :------    |
| SSL001 | asn1   | OOB reads + writes (size 1) | base       | no         |
| SSL002 | client | UAF write (size 315)        | size, data | size, data |
| SSL009 | x509   | OOB read (size 2)           | base       | no         |

Comments:
- SSL001: looks like some sort of copy (repeated oob reads and writes in a loop). some later writes have controlled data (random controlled bytes in memory?)

## "new" bugs

| Bug       | Fuzzer | Type                 | Taint      | Control    | Patch |
| :--       | :----- | :--:                 | :----      | :------    | :---- |
| SSLNEW001 | client | UAF writes (size 1)  | yes        | size       | fixed |
| SSLNEW002 | client | UAF write (size 296) | size, data | size, data | fixed |
| SSLNEW003 | client | UAF write (size 429) | size, data | size, data | fixed |
| SSLNEW004 | server | UAF write (size 102) | size, data | size       | fixed |
| SSLNEW005 | server | UAF write (size 253) | size, data | size, data | fixed |
| SSLNEW006 | server | UAF write (size 226) | size, data | size       | fixed |

Comments:
- SSLNEW002: similar to SSL002
- SSLNEW003: similar to SSL002
- SSLNEW004: similar to SSLNEW002, but no control on written data
- SSLNEW005: similar to SSL002
- SSLNEW006: similar to SSLNEW004

# libtiff

| Bug      | Fuzzer | Type                        | Taint      | Control    |
| :--      | :----- | :--:                        | :----      | :------    |
| TIF001   | rgba   | OOB reads + writes (size 1) | base       | no         |
| TIF002   | rgba   | OOB write (size 32) + reads | yes        | data       |
| TIF002_2 | rgba   | OOB write (size 82) + reads | yes        | data, size |
| TIF008   | rgba   | OOB reads + writes (size 1) | base, data | no         |
| TIF008_2 | rgba   | OOB reads + writes (size 1) | base, data | no         |

Comments:
- TIF001: looks like some sort of copy, maybe in a loop. no control but loop condition may be controlled
- TIF008_2: same as TIF008 but with different values. bug may be somewhat controllable but with very different inputs

# sqlite3

| Bug    | Fuzzer         | Type                       | Taint | Control |
| :--    | :-----         | :--:                       | :---- | :------ |
| SQL018 | sqlite3_fuzzer | OOB writes (size 14 +1 +1) | no    | no      |

Comments:
- SQL018: crash is inconsistent with GDB. however oobcheck reveals memory corruptions missed by asan and valgrind. the crash is likely a later consequence of that. the size of the first write could be controlled by the length of trailing quotation marks in SQL queries based on bug reports.

# libpng

| Bug    | Fuzzer             | Type                    | Taint | Control |
| :--    | :-----             | :--:                    | :---- | :------ |
| PNG007 | libpng_read_fuzzer | read at invalid address | no    | no      |

Comments:
- PNG007: report says that the bug is due to a zero-size object. the lack of control thus makes sense.

# php

| Bug    | Fuzzer          | Type                     | Taint      | Control |
| :--    | :-----          | :--:                     | :----      | :------ |
| PHP011 | php_fuzzer-exif | heap OOB read (size 768) | base, size | size    |
