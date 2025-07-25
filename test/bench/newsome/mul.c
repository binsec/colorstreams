int main()
{
    unsigned int i = 0;
    _pinstrio_source_(&i, 4, "i:controlled");
    unsigned int j = i * 2;
    _pinstrio_sink_(&j, 4, "j");
    return 0;
}
