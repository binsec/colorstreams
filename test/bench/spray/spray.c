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

void spray(char *ptr)
{

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
    spray(foo);
    callfun(1);
    return 0;
}
