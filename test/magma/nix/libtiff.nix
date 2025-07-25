{stdenv, fetchgit, fetchFromGitHub, symlinkJoin, lib, autoconf, automake, autogen, libtool, pkg-config, cmake, extra-cmake-modules, nasm, zlib, xz, libjpeg, pinstrio_stubs, fuzzer_main}:
    let
        magma_tiff = fetchgit
            {
                url = "https://github.com/HexHive/magma";
                rev = "75d1ae7b180443a778b8830c79176ca5f93642ac";
                sparseCheckout = ["targets/libtiff"];
                sha256 = "sha256-wg/UYEYXFS9GAQMqsta2lKVTo7aSRF+v5OuXlOib9eQ=";
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
                        name = "libtiff_magma";
                        src = fetchgit
                            {
                                url = "https://gitlab.com/libtiff/libtiff";
                                rev = "c145a6c14978f73bb484c955eb9f84203efcb12e";
                                sha256 = "sha256-70cQGtLg0ILpO1/DpaDcqKW2vSI6V7nWisjxZWqwhuc=";
                            };
                        version = "c145a6c";
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
                                nasm
                                zlib
                                xz
                                libjpeg
                                pinstrio_stubs
                            ];
                        patches = [(builtins.toPath "${magma_tiff}/targets/libtiff/patches/setup/libtiff.patch") (builtins.toPath "${magma_tiff}/targets/libtiff/patches/bugs/*.patch")];
                        configurePhase =
                            ''
                                mkdir config
                                cp ${libtool}/share/libtool/build-aux/config.guess ./config/config.guess.tmp
                                cp ${libtool}/share/libtool/build-aux/config.sub ./config/config.sub.tmp
                                touch wget
                                export PATH=".:$PATH"
                                chmod +x wget
                                ./wget
                                ls -l config
                                ./autogen.sh
                                export CFLAGS="$CFLAGS -ggdb ${asan_opt} ${enable_fixes}"
                                export CXXFLAGS="$CXXFLAGS -ggdb ${asan_opt} ${enable_fixes}"
                                export LDLAGS="$LDLAGS ${asan_opt} ${enable_fixes}"
                                ./configure --disable-shared
                            '';
                        buildPhase =
                            ''
                                make -j$(nproc) clean
                                make -j$(nproc)
                                cp tools/tiffcp libtiff_tiffcp${asan_suff}${fixes_suff}.run
                                $CC $CFLAGS -c -I${pinstrio_stubs}/include -DPINSTRIO ${fuzzer_main}/fuzzer_main.c -o fuzzer_main.o
                                $CXX $CXXFLAGS -std=c++11 -c -Iinclude -Ilibtiff ${magma_tiff}/targets/libtiff/src/tiff_read_rgba_fuzzer.cc -o tiff_read_rgba_fuzzer.o
                                $CXX $CXXFLAGS -std=c++11 tiff_read_rgba_fuzzer.o fuzzer_main.o -o libtiff_tiff_read_rgba_fuzzer${asan_suff}${fixes_suff}.run libtiff/.libs/libtiffxx.a libtiff/.libs/libtiff.a -lz -ljpeg -llzma -L${pinstrio_stubs}/lib -lpinstrio $LDFLAGS $LIBS
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
            name = "magma_libtiff_fuzzers";
            paths = (map (d: d.out) [(mk_fuzzers true false) (mk_fuzzers false false) (mk_fuzzers true true) (mk_fuzzers false true)]);
        }
