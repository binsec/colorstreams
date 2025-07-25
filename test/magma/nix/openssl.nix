{stdenv, fetchgit, fetchFromGitHub, symlinkJoin, lib, autoconf, automake, autogen, libtool, pkg-config, perl, pinstrio_stubs, fuzzer_main}:
    let
        magma_openssl = fetchgit
            {
                url = "https://github.com/HexHive/magma";
                rev = "75d1ae7b180443a778b8830c79176ca5f93642ac";
                sparseCheckout = ["targets/openssl"];
                sha256 = "sha256-sr3XlR4FzaAbORka0Qi6OyB4b3ZHfW7Nh3s/izKwaVw=";
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
                        name = "openssl_magma";
                        src = fetchFromGitHub
                            {
                                owner = "openssl";
                                repo = "openssl";
                                rev = "3bd5319b5d0df9ecf05c8baba2c401ad8e3ba130";
                                sha256 = "sha256-JAcAxUGWRQbfMQWUW2H6vaoQ2TOHqV/Fnoy029wUSmM=";
                            };
                        version = "3bd5319";
                        nativeBuildInputs =
                            [
                                libtool
                                pkg-config
                                autoconf
                                automake
                                autogen
                                perl
                            ];
                        buildInputs =
                            [
                                pinstrio_stubs
                            ];
                        patches = [(builtins.toPath "${magma_openssl}/targets/openssl/patches/bugs/*.patch")];
                        configurePhase =
                            ''
                                cp ${magma_openssl}/targets/openssl/src/abilist.txt .
                                export CFLAGS="$CFLAGS -ggdb ${asan_opt} ${enable_fixes}"
                                export CXXFLAGS="$CXXFLAGS -ggdb ${asan_opt} ${enable_fixes}"
                                export LDLAGS="$LDLAGS ${asan_opt} ${enable_fixes}"
                                patchShebangs --build Configure
                                ./Configure --debug disable-tests -DPEDANTIC \
                                    -DFUZZING_BUILD_MODE_UNSAFE_FOR_PRODUCTION no-shared no-module \
                                    enable-tls1_3 enable-rc5 enable-md2 enable-ec_nistp_64_gcc_128 enable-ssl3 \
                                    enable-ssl3-method enable-nextprotoneg enable-weak-ssl-ciphers \
                                    ${asan_conf} $CFLAGS ${enable_fixes}
                            '';
                        buildPhase =
                            ''
                                make -j$(nproc) clean
                                make -j$(nproc) LDCMD="$CXX $CXXFLAGS"
                                $CC $CFLAGS -ggdb -c -I${pinstrio_stubs}/include -DNO_LLVM -DPINSTRIO ${fuzzer_main}/fuzzer_main.c -o fuzzer_main.o
                                $CC $CFLAGS -ggdb -c -Iinclude -Ifuzz fuzz/fuzz_rand.c -o fuzz_rand.o
                                for fuzzer in asn1 asn1parse bignum server client x509; do \
                                    $CC $CFLAGS -ggdb -Iinclude -Ifuzz -c fuzz/$fuzzer.c -o $fuzzer.o; \
                                    $CXX $CXXFLAGS -ggdb -std=c++11 $fuzzer.o fuzz_rand.o fuzzer_main.o -o openssl_''${fuzzer}${asan_suff}${fixes_suff}.run ./libssl.a ./libcrypto.a $LDFLAGS $LIBS -L${pinstrio_stubs}/lib -lpinstrio; \
                                done
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
            name = "magma_openssl_fuzzers";
            paths = (map (d: d.out) [(mk_fuzzers true false) (mk_fuzzers false false) (mk_fuzzers true true) (mk_fuzzers false true)]);
        }
