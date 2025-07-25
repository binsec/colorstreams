int main()
{
    unsigned int i = 0;
    _pinstrio_source_(&i, 4, "i:controlled");
    i = ((i >> 16) ^ i) & 0xffff;
    i = i | (i << 16);
    _pinstrio_sink_(&i, 4, "i");
    return 0;
}
