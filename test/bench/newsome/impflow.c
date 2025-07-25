int main()
{
    int input = 1;
    _pinstrio_source_(&input, 4, "input:controlled");
    int output = 0;
    if(input == 1) output = 1;
    if(input == 2) output = 2;
    if(input == 3) output = 3;
    if(input == 4) output = 4;
    if(input == 5) output = 5;
    if(input == 6) output = 6;
    _pinstrio_sink_(&output, 4, "output");
    return 0;
}
