#include "stdio.h"
#include "stdlib.h"
#include "stdint.h"
#include "unistd.h"

void notcoolfun()
{
    printf("not cool!\n");
}

typedef struct type1
{
    int i;
}
type1_t;

typedef struct type2
{
    type1_t t1[8];
    uint64_t option;
}
type2_t;

typedef struct type3
{
    int (*foo)();
}
type3_t;

type1_t *vul = NULL;
type3_t *tgt = NULL;

void sys_socket()
{
    vul = (type1_t *) malloc(sizeof(type1_t));
}

void sys_setsockopt(uint64_t opt)
{
    ((type2_t *) vul)->option = opt;
}

void sys_create_tgt()
{
    tgt = (type3_t *) malloc(sizeof(type3_t));
    tgt->foo = NULL;
}

void sys_deref()
{
    if(tgt->foo)
    {
        _pinstrio_sink_(&tgt->foo, 8, "foo");
        tgt->foo();
    }
}

int main()
{
    printf("Uncool function at %p!\n", &notcoolfun);
    uint64_t opt = &notcoolfun;
    _pinstrio_source_(&opt, 8, "controlled");
    sys_socket();
    sys_create_tgt();
    sys_setsockopt(opt);
    printf("Going to %p!\n",tgt->foo);
    sys_deref();
}
