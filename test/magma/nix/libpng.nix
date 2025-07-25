{stdenv, fetchgit, fetchurl, fetchFromGitHub, symlinkJoin, lib, autoconf, automake, autogen, libtool, pkg-config, zlib, pinstrio_stubs, fuzzer_main}:
    let
        magma_png = fetchgit
            {
                url = "https://github.com/HexHive/magma";
                rev = "75d1ae7b180443a778b8830c79176ca5f93642ac";
                sparseCheckout = ["targets/libpng"];
                sha256 = "sha256-fASUfU9s/2G/9Zell1PgcOfW7cj7DClF0zIBl6RXx8w=";
            };
        mk_fuzzers =
            asan: fixes:
                let
                    asan_suff = if asan then "_asan" else "";
                    asan_conf = if asan then "enable-asan" else "";
                    asan_opt = if asan then "-fsanitize=address" else "";
                    enable_fixes = if fixes then "-DMAGMA_ENABLE_FIXES" else "";
                    fixes_suff = if fixes then "_fixed" else "";
                in
                stdenv.mkDerivation
                    {
                        name = "libpng_magma";
                        src = fetchFromGitHub
                            {
                                owner = "pnggroup";
                                repo = "libpng";
                                rev = "a37d4836519517bdce6cb9d956092321eca3e73b";
                                sha256 = "sha256-KCpOY1kL4eG51bUv28aw8jTjUNwr3UHAGBqAaN2eBvg=";
                            };
                        version = "a37d483";
                        nativeBuildInputs =
                            [
                                libtool
                                pkg-config
                                autoconf
                                automake
                                autogen
                            ];
                        buildInputs =
                            [
                                zlib
                            ];
                        patches = [(builtins.toPath "${magma_png}/targets/libpng/patches/setup/*.patch") (builtins.toPath "${magma_png}/targets/libpng/patches/bugs/*.patch")];
                        configurePhase =
                            ''
                                export CFLAGS="$CFLAGS -ggdb ${asan_opt} ${enable_fixes}"
                                export CXXFLAGS="$CXXFLAGS -ggdb ${asan_opt} ${enable_fixes}"
                                export LDFLAGS="$LDFLAGS -ggdb ${asan_opt} ${enable_fixes}"
                                autoreconf -f -i
                                ./configure --disable-shared
                            '';
                        buildPhase =
                            ''
                                make -j$(nproc) clean
                                make -j$(nproc) libpng16.la
    
                                $CXX $CXXFLAGS -std=c++11 -c -I. contrib/oss-fuzz/libpng_read_fuzzer.cc -o libpng_read_fuzzer.o
                                $CC $CFLAGS -c -I${pinstrio_stubs}/include -DPINSTRIO ${fuzzer_main}/fuzzer_main.c -o fuzzer_main.o
                                $CXX $CXXFLAGS libpng_read_fuzzer.o fuzzer_main.o -o libpng_read_fuzzer${asan_suff}${fixes_suff}.run $LDFLAGS -L${pinstrio_stubs}/lib -lpinstrio .libs/libpng16.a $LIBS -lz
                            '';
                        installPhase =
                            ''
                                install -Dt $out/bin *.run
                            '';
                        dontStrip = true;
                    };
    in
    symlinkJoin
        {
            name = "magma_libpng_fuzzers";
            paths = (map (d: d.out) [(mk_fuzzers true false) (mk_fuzzers false false) (mk_fuzzers true true) (mk_fuzzers false true)]);
        }
