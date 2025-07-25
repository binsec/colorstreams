mk_dir()
{
    mkdir out
}

mk_dir || true
for poc in ${@:2}
do
    echo $poc
    touch out/$(basename $poc).out
    timeout -v 1 $1 $poc > out/$(basename $poc).out 2>&1
done
