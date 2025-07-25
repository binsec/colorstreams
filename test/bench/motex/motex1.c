#include "stdint.h"
#include "string.h"

#define HEADER_SIZE 40

uint64_t check_header(char *input, uint64_t input_size)
{
    //2) input[0->7] sprayed on the stack
    uint64_t header = *((uint64_t *) input);
    return header <= 296;
}
    
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
    _pinstrio_sink_(&size, 8, "size");
    _pinstrio_constrained_sink_(&of_size, 8, "of_wsize", &oob, 4);
    _pinstrio_abort_();
    memcpy(buf, input + HEADER_SIZE, size);
}

int main()
{
    //1) inputs: char *input, uint64_t input_size
    uint64_t input = 0;
    uint64_t input_size = 0;
    _pinstrio_source_(&input_size, 8, "controlled");
    char buf[256];
    if(check_header((char *) &input, input_size))
        get_msg(buf, 256, (char *) &input, input_size);
    return 0;
}
