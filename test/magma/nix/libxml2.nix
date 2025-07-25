{stdenv, fetchgit, fetchFromGitHub, symlinkJoin, lib, autoconf, automake, autogen, libtool, pkg-config, zlib, xz, pinstrio_stubs, fuzzer_main}:
    let
        magma_libxml2 = fetchgit
            {
                url = "https://github.com/HexHive/magma";
                rev = "75d1ae7b180443a778b8830c79176ca5f93642ac";
                sparseCheckout = ["targets/libxml2"];
                sha256 = "sha256-9yvL7rs5YwWVAopn5ooYJsmQEccFyJV1uKYsoQApZoY=";
            };
        mk_fuzzers =
            asan:
                let
                    asan_suff = if asan then "_asan" else "";
                    asan_opt = if asan then " -fsanitize=address" else "";
                in
                stdenv.mkDerivation
                    {
                        name = "libxml2_magma";
                        src = fetchFromGitHub
                            {
                                owner = "GNOME";
                                repo = "libxml2";
                                rev = "ec6e3efb06d7b15cf5a2328fabd3845acea4c815";
                                sha256 = "sha256-5IGX77c2/BkyJWTyIoPKPs/TZ/2DQ4lpPdzKgogKWPY=";
                            };
                        version = "ec6e3ef";
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
                                xz
                                pinstrio_stubs
                            ];
                        patches = [(builtins.toPath "${magma_libxml2}/targets/libxml2/patches/setup/*.patch") (builtins.toPath "${magma_libxml2}/targets/libxml2/patches/bugs/*.patch")];
                        configurePhase =
                            ''
                                export CFLAGS="$CFLAGS -ggdb${asan_opt}" 
                                export CXXFLAGS="$CXXFLAGS -ggdb${asan_opt}" 
                                export LDFLAGS="$LDFLAGS${asan_opt}"
                                ./autogen.sh --disable-shared --with-lzma --without-http --without-python --without-threads
                            '';
                        buildPhase =
                            ''
                                make -j$(nproc) clean
                                make -j$(nproc) all
                                mv xmllint libxml2_xmllint${asan_suff}.run
                                $CC $CFLAGS -ggdb -c -I${pinstrio_stubs}/include -DPINSTRIO ${fuzzer_main}/fuzzer_main.c -o fuzzer_main.o
                                $CXX $CXXFLAGS -ggdb -std=c++11 -Iinclude -I${magma_libxml2}/targets/libxml2/src -c ${magma_libxml2}/targets/libxml2/src/libxml2_xml_read_memory_fuzzer.cc -o read_memory_fuzzer.o
                                $CXX $CXXFLAGS -ggdb -std=c++11 read_memory_fuzzer.o fuzzer_main.o -o libxml2_read_memory_fuzzer${asan_suff}.run .libs/libxml2.a $LDFLAGS $LIBS -lz -llzma -L${pinstrio_stubs}/lib -lpinstrio
                            '';
                        installPhase =
                            ''
                                install -Dt $out/bin libxml2_xmllint${asan_suff}.run
                                install -Dt $out/bin libxml2_read_memory_fuzzer${asan_suff}.run
                            '';
                        dontStrip = true;
                    };
    in
    symlinkJoin
        {
            name = "magma_libxml2_fuzzers";
            paths = (map (d: d.out) [(mk_fuzzers true) (mk_fuzzers false)]);
        }
