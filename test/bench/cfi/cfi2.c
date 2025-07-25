#include "stdio.h"

void *callers[2], *callees[2];

int cfi(void *caller, void *callee)
{
    return 
        (caller == callers[0] && callee == callees[0]) ||
        (caller == callers[1] && callee == callees[1]);
}

void coolfun()
{
    printf("cool!\n");
}

void notcoolfun()
{
    printf("not cool!\n");
}

void f()
{
    notcoolfun();
}

int main()
{
    callers[0] = &main;
    callers[1] = &f;
    callees[0] = &coolfun;
    callees[1] = &notcoolfun;
    void (*caller)() = &main;
    void (*callee)() = &coolfun;
    _pinstrio_source_(&callee, 8, "callee:controlled");
    if(cfi(caller, callee))
    {
        _pinstrio_sink_(&callee, 8, "callee");
        callee();
    }
    return 0;
}
