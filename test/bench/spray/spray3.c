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
        {
            res = 0;
            break;
        }
        res *= n;
        p --;
    }
    return res;
}

int64_t spow(int64_t n, uint64_t p)
{
    uint64_t sign = ((uint64_t) n) & (1ULL << 63);
    uint64_t res = pow(((uint64_t) n) - sign, p);
    if(sign)
        return -res;
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
    int64_t n = -42;
    _pinstrio_source_(&n, 8, "controlled");
    spow(n, 16);
    callfun(1);
    return 0;
}
