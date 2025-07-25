#include "stdio.h"
#include "stdlib.h"
#include "string.h"
#include "unistd.h"

typedef struct padded_fun
{
    int pad1[4];
    void (*foo)();
    int pad2[4];
}
padded_fun_t;

void notcoolfun()
{
    printf("not cool!\n");
}

char *get_input()
{
    char *input = (char *) malloc(40);
    void (**tmp)() = (void (**)()) input;
    tmp[0] = 0xdeadbeefdeadbeef;
    tmp[1] = 0xdeadbeefdeadbeef;
    tmp[2] = &notcoolfun;
    tmp[3] = 0xdeadbeefdeadbeef;
    _pinstrio_source_(&tmp[2], 8, "controlled");
    return input;
}

int main()
{
    char *s = (char *) malloc(32);
    char *input = get_input();
    //strcpy(s, input);
    memcpy(s, input, 32);
    free(s);
    padded_fun_t *pfun = malloc(sizeof(padded_fun_t));
    _pinstrio_sink_(&pfun->foo, 8, "foo");
    pfun->foo();
    free(pfun);
    return 0;
}
