#include "stdlib.h"
#include "stdio.h"
#include "string.h"
#include "time.h"
#include "stdint.h"
#include "unistd.h"

typedef struct field
{
    unsigned int h, w, m, left, dead;
    char *mines;
    char *numbers;
    char *reveal;
}
field_t;

field_t *f;

char *exploit_string;

void add_mines()
{
    for(int i = 0; i < f->m; i++)
    {
        int choice = rand() % (f->left - i);
        int pos = 0;
        while(choice > 0)
        {
            if(f->mines[pos] == 0)
                choice--;
            pos = (pos + 1) % f->left;
        }
        while(f->mines[pos] != 0)
            pos = (pos + 1) % f->left;
        f->mines[pos] = 1;
        if(pos % f->w > 0)
        {
            f->numbers[pos - 1]++;
            if(pos >= f->w)
                f->numbers[pos - f->w - 1]++;
            if(pos < f->left - f->w)
                f->numbers[pos + f->w - 1]++;
        }
        if(pos % f->w < f->w - 1)
        {
            f->numbers[pos + 1]++;
            if(pos >= f->w)
                f->numbers[pos - f->w + 1]++;
            if(pos < f->left - f->w)
                f->numbers[pos + f->w + 1]++;
        }
        if(pos >= f->w)
            f->numbers[pos - f->w]++;
        if(pos < f->left - f->w)
            f->numbers[pos + f->w]++;
    }
}

void draw()
{
    _pinstrio_concrete_begin_();
    for(int i = 0; i < f->h; i++)
    {
        for(int j = 0; j < f->w; j++)
        {
            int index = i * f->w + j;
            switch(f->reveal[index])
            {
                case 0:
                    printf("[ ]");
                    break;
                case 1:
                    if(f->mines[index])
                        printf("[*]");
                    else
                    {
                        if(f->numbers[index] > 0)
                            printf(" %hhd ", f->numbers[index]);
                        else
                            printf("   ");
                    }
                    break;
                case 2:
                    printf("[P]");
                    break;
            }
        }
        printf("\n");
    }
    _pinstrio_concrete_end_();
}

void parse_cmd(char *cmd)
{
    if(f->dead)
        return;
    char *tok1 = strtok(cmd," ");
    //_pinstrio_source_(tok1, strlen(tok1), "cmd:controlled");
    char *tok2 = strtok(NULL," ");
    //_pinstrio_source_(tok2, strlen(tok2), "x:controlled");
    char *tok3 = strtok(NULL," ");
    //_pinstrio_source_(tok3, strlen(tok3), "y:controlled");
    if(tok1 && tok2 && tok3)
    {
        int x = atoi(tok2);
        int y = atoi(tok3);
        int index = y * f->w + x;
        switch(tok1[0])
        {
            case 'c':
                if(f->reveal[index] != 1)
                {
                    int oob = index < 0;
                    int uf_woff = - index;
                    _pinstrio_sink_(&index, 4, "index");
                    _pinstrio_constrained_sink_(&uf_woff, 4, "uf_woff", &oob, 4);
                    f->reveal[index] = 1;
                    f->left--;
                    if(f->mines[index])
                        f->dead = 1;
                }
                return;
            case 'p':
                if(f->reveal[index] != 1)
                    f->reveal[index] = 2;
                return;
        }
    }
    printf("Command format: <action> <x> <y>\n-> actions: c (clear), f (flag)\n");
}

void input()
{
    char buf[8];
    sscanf("c 4 -28\n", " %[^\n]", &buf);
    _pinstrio_source_(buf, 8, "controlled");
    parse_cmd(buf);
}

void lost()
{
    printf("You lost!\n");
    memset(f->reveal, 1, f->w * f->h);
    draw();
}

void won()
{
    _pinstrio_concrete_begin_();
    printf("You won!\n");
    memset(f->reveal, 1, f->w * f->h);
    draw();
    _pinstrio_concrete_end_();
}

int main(int argc, char **argv)
{
    /*if(argc != 4)
    {
        printf("Usage: minesweeper <height> <width> <nmines>\n");
        return 0;
    }*/

    f = (field_t *) malloc(sizeof(field_t));
    f->h = 10;
    f->w = 10;
    f->m = 10;
    f->left = f->h * f->w;
    f->dead = 0;

    if(f->left < f->m)
    {
        printf("Too many mines!\n");
        return 0;
    }

    f->mines = (char *) calloc(f->left, 1);
    f->numbers = (char *) calloc(f->left, 1);
    f->reveal = (char *) calloc(f->left, 1);

    srand(time(NULL));

    add_mines();
    
    while(1)
    {
        draw();
        input();
        if(f->dead)
        {
            lost();
            return 0;
        }
        //_pinstrio_sink_(&f->left, 4, "f->left");
        //_pinstrio_sink_(f, sizeof(field_t), "field");
        if(f->left <= f->m)
        {
            won();
            return 0;
        }
    }
    return 0;
}
