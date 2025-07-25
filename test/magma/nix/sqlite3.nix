{stdenv, fetchgit, fetchurl, fetchFromGitHub, symlinkJoin, lib, autoconf, automake, autogen, libtool, pkg-config, zlib, tcl, pinstrio_stubs, fuzzer_main}:
    let
        magma_sqlite3 = fetchgit
            {
                url = "https://github.com/HexHive/magma";
                rev = "75d1ae7b180443a778b8830c79176ca5f93642ac";
                sparseCheckout = ["targets/sqlite3"];
                sha256 = "sha256-Rq8fAtZrd4hOE1FOONr2WMfyHaRpzoYjs9euGRIEDDA=";
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
                        name = "sqlite3_magma";
                        src = fetchurl
                            {
                                url = "https://www.sqlite.org/src/tarball/sqlite.tar.gz?r=8c432642572c8c4b";
                                sha256 = "sha256-QSuRUSlH8YZQ0qHG1QlgxKdRuU/81uaPuFzYFpK6cKA=";
                            };
                        version = "magma";
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
                                tcl
                            ];
                        patches = [(builtins.toPath "${magma_sqlite3}/targets/sqlite3/patches/setup/*.patch") (builtins.toPath "${magma_sqlite3}/targets/sqlite3/patches/bugs/*.patch")];
                        configurePhase =
                            ''
                                export CFLAGS="$CFLAGS -ggdb ${asan_opt} ${enable_fixes} -DSQLITE_MAX_LENGTH=128000000 \
                                   -DSQLITE_MAX_SQL_LENGTH=128000000 \
                                   -DSQLITE_MAX_MEMORY=25000000 \
                                   -DSQLITE_PRINTF_PRECISION_LIMIT=1048576 \
                                   -DSQLITE_DEBUG=1 \
                                   -DSQLITE_MAX_PAGE_COUNT=16384"
                                export LDFLAGS="$LDFLAGS -ggdb ${asan_opt}"

                                ./configure --disable-shared --enable-rtree
                            '';
                        buildPhase =
                            ''
                                make -j$(nproc) clean
                                make -j$(nproc)
                                make sqlite3.c
    
                                $CC $CFLAGS -c -I. test/ossfuzz.c -o sqlite3_fuzzer.o
                                $CC $CFLAGS -c -I${pinstrio_stubs}/include -DPINSTRIO ${fuzzer_main}/fuzzer_main.c -o fuzzer_main.o
                                $CC $CFLAGS sqlite3.o sqlite3_fuzzer.o fuzzer_main.o -o sqlite3_fuzzer${asan_suff}${fixes_suff}.run $LDFLAGS $LIBS -L${pinstrio_stubs}/lib -lpinstrio -pthread -ldl -lm
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
            name = "magma_sqlite3_fuzzers";
            paths = (map (d: d.out) [(mk_fuzzers true false) (mk_fuzzers false false) (mk_fuzzers true true) (mk_fuzzers false true)]);
        }
