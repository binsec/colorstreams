#include "stdlib.h"
#include "stdio.h"
#include "stdint.h"
#ifdef PINSTRIO
#include "pinstrio.h"
#endif

int main(int argc, char **argv)
{
    if(argc != 2)
        return 1;

    FILE *f = fopen(argv[1], "r");
    fseek(f, 0, SEEK_END);
    ssize_t size = ftell(f);
    rewind(f);

    uint8_t *data = (uint8_t *) malloc(size + 1);
    fread(data, size, 1, f);
    fclose(f);

#ifdef PINSTRIO
    _pinstrio_source_(data, size, "fuzzer_input:controlled");
#endif
#ifdef FUZZ_INIT
#ifdef NO_LLVM
    FuzzerInitialize(NULL, NULL);
#else
    LLVMFuzzerInitialize(NULL, NULL);
#endif
#endif
#ifdef NO_LLVM
    FuzzerTestOneInput(data, size);
#else
    LLVMFuzzerTestOneInput(data, size);
#endif

    free(data);

    return 0;
}
