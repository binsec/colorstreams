int main()
{
    int a = 0;
    int b = 0;
    _pinstrio_source_(&a, 4, "a:controlled");
    _pinstrio_source_(&b, 4, "b:controlled");
    int c = a + b;
    _pinstrio_sink_(&c, 4, "c");
    return 0;
}
