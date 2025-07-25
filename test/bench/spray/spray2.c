#include "stdlib.h"
#include "stdio.h"
#include "unistd.h"
#include "stdint.h"

void coolfun()
{
    printf("cool!\n");
}

void notcoolfun()
{
    printf("not cool!\n");
}

uint64_t pow(uint64_t n, uint64_t p)
{
    uint64_t res = 1;
    uint64_t max = -1;
    while(p > 0)
    {
        if(max / n < res)
            return 0;
        res *= n;
        p --;
    }
    return res;
}

void callfun(uint64_t n)
{
    void (*foo)();
    switch(n)
    {
        case 0:
            foo = &coolfun;
            break;
    }
    if(foo)
    {
        _pinstrio_sink_(&foo, 8, "foo");
        foo();
    }
}

int main()
{
    void (*foo)() = &notcoolfun;
    _pinstrio_source_(&foo, 8, "controlled");
    pow(foo, 16);
    callfun(1);
    return 0;
}
