#include "stdlib.h"
#include "stdio.h"
#include "string.h"
#include "time.h"

typedef struct field
{
    unsigned int h, w, m, left, dead;
    char *mines;
    char *numbers;
    char *reveal;
}
field_t;

field_t *f;

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
}

void cascade_reveal(int index)
{
    if(f->reveal[index] == 1)
        return;
    f->reveal[index] = 1;
    f->left--;
    if(f->mines[index])
        f->dead = 1;
    else
    {
        if(f->numbers[index] == 0)
        {
            if(index % f->w > 0)
            {
                cascade_reveal(index - 1);
                if(index >= f->w)
                    cascade_reveal(index - f->w - 1);
                if(index < f->w * f->h - f->w)
                    cascade_reveal(index + f->w - 1);
            }
            if(index % f->w < f->w - 1)
            {
                cascade_reveal(index + 1);
                if(index >= f->w)
                    cascade_reveal(index - f->w + 1);
                if(index < f->w * f->h - f->w)
                    cascade_reveal(index + f->w + 1);
            }
            if(index >= f->w)
                cascade_reveal(index - f->w);
            if(index < f->w * f->h - f->w)
                cascade_reveal(index + f->w);
        }
    }
}

void parse_cmd(char *cmd)
{
    if(f->dead)
        return;
    char *tok1 = strtok(cmd," ");
    char *tok2 = strtok(NULL," ");
    char *tok3 = strtok(NULL," ");
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
                    f->reveal[index] = 1;
                    f->left--;
                    if(f->mines[index])
                        f->dead = 1;
                }
                return;
            case 'C':
                cascade_reveal(index);
                return;
            case 'p':
                if(f->reveal[index] != 1)
                    f->reveal[index] = 2;
                return;
        }
    }
    printf("Command format: <action> <x> <y>\n-> actions: c (clear), C (cascade clear), f (flag)\n");
}

void input()
{
    char buf[8];
    scanf(" %[^\n]", buf);
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
    printf("You won!\n");
    memset(f->reveal, 1, f->w * f->h);
    draw();
}

int main(int argc, char **argv)
{
    if(argc != 4)
    {
        printf("Usage: minesweeper <height> <width> <nmines>\n");
        return 0;
    }

    f = (field_t *) malloc(sizeof(field_t));
    f->h = atoi(argv[1]);
    f->w = atoi(argv[2]);
    f->m = atoi(argv[3]);
    f->left = f->h * f->w;
    f->dead = 0;

    if(f->left - 9 < f->m)
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
        if(f->left <= f->m)
        {
            won();
            return 0;
        }
    }
    return 0;
}
