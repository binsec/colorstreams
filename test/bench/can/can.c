#include "stdlib.h"
#include "stdio.h"
#include "stdint.h"
#include "unistd.h"

typedef struct protected_fun
{
    uint64_t canary;
    void (*foo)();
}
profun;

void coolfun()
{
    printf("cool!\n");
}

void notcoolfun()
{
    printf("not cool!\n");
}

profun *make_profun(void (*foo)())
{
    profun *pfun = (profun *) malloc(sizeof(profun));
    pfun->canary = 42;
    pfun->foo = foo;
    return pfun;
}

uint64_t get_input()
{
    return &notcoolfun;
}

int main()
{
    profun *pfun = make_profun(&coolfun);
    uint64_t *vuln = (uint64_t *) malloc(4 * sizeof(uint64_t));
    for(int i=0; i<4; i++)
    {
        vuln[-i] = get_input();
        _pinstrio_source_(&vuln[-i], 8, "input:controlled");
    }
    _pinstrio_sink_(&pfun->canary, 8, "canary");
    _pinstrio_sink_(&pfun->foo, 8, "foo");
    if(pfun->canary == 42)
        pfun->foo();
    return 0;
}
