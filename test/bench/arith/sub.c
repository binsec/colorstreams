int main()
{
    volatile int a = 0;
    _pinstrio_source_(&a, 4, "a:controlled");
    int b = a - a;
    _pinstrio_sink_(&b, 4, "b");
    return 0;
}
