#include "stdio.h"
#include "stdint.h"
#include "string.h"

#define HEADER_SIZE 40

//checks that the message header is valid
uint64_t check_header(char *input, uint64_t input_size)
{
    //2) header sprayed on the stack
    uint64_t id = *((uint64_t *) input) & 0xfff;
    if(id <= 296)
    {
        uint64_t a = ((uint64_t *) input)[1];
        uint64_t b = ((uint64_t *) input)[2];
        //...
        return 1;
    }
    return 0;
}
    
//copies the message into a buffer
void get_msg(char *buf, uint64_t buf_size, 
        char *input, uint64_t input_size)
{
    //3) not initialized => size = sprayed header
    uint64_t size;
    if(input_size <= buf_size + HEADER_SIZE)
        //4) input_size < 40 => integer underflow
        size = input_size - HEADER_SIZE;
    //5) buffer overflow!!!
    // a. input_size < 40: 2^64 - 40 <= size < 2^64
    // b. input_size > 296: size = header 
    int oob = size > buf_size;
    uint64_t of_size = size - buf_size;
    printf("copying %llu bytes into a %llu bytes buffer...\n", size, buf_size);
    _pinstrio_sink_(&size, 8, "size");
    _pinstrio_constrained_sink_(&of_size, 8, "of_wsize", &oob, 4);
    _pinstrio_abort_();
    memcpy(buf, input + HEADER_SIZE, size);
    printf("done\n");
}

//parses command line inputs
uint64_t get_input(int argc, char **argv, char **msg)
{
    if(argc != 3)
    {
        printf("Usage: target message message_size\n");
        exit(0);
    }
    *msg = argv[1];
    return atoi(argv[2]);
}

int main(int argc, char **argv)
{
    //1) inputs: char *input, uint64_t input_size
    char *input = NULL;
    uint64_t input_size = get_input(argc, argv, &input);
    _pinstrio_source_(input, strlen(input), "message:controlled");
    _pinstrio_source_(&input_size, 8, "size:controlled");
    char buf[256];
    if(check_header(input, input_size))
        get_msg(buf, 256, input, input_size);
    else
        printf("invalid message header\n");
    return 0;
}
