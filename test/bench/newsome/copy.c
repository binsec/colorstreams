int main()
{
    int i = 0;
    _pinstrio_source_(&i, 4, "i:controlled");
    int j = i;
    _pinstrio_sink_(&j, 4, "j");
    return 0;
}
