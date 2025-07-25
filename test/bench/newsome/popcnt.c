int main()
{
    unsigned int i = 0;
    _pinstrio_source_(&i, 4, "i:controlled");
    i = (i & 0x55555555) + ((i >> 1) & 0x55555555);
    i = (i & 0x33333333) + ((i >> 2) & 0x33333333);
    i = (i & 0x0f0f0f0f) + ((i >> 4) & 0x0f0f0f0f);
    i = (i & 0x00ff00ff) + ((i >> 8) & 0x00ff00ff);
    i = (i + (i >> 16)) & 0xffff;
    _pinstrio_sink_(&i, 4, "i");
    return 0;
}
