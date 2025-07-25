{stdenv, fetchgit, fetchFromGitHub, symlinkJoin, lib, autoconf, automake, autogen, libtool, pkg-config, perl, pinstrio_stubs, fuzzer_main}:
    let
        mk_fuzzers =
            asan:
                let
                    asan_suff = if asan then "_asan" else "";
                    asan_conf = if asan then "enable-asan" else "";
                    asan_opt = if asan then "-fsanitize=address" else "";
                in
                stdenv.mkDerivation
                    {
                        name = "openssl";
                        src = fetchFromGitHub
                            {
                                owner = "openssl";
                                repo = "openssl";
                                rev = "981d129a5609ee2e031367c34c67a9f61a5bfd66";
                                sha256 = "sha256-1pMBuuK/ntIuTYWjEC2943S11wyPQDRGGSm+5pWBWco=";
                            };
                        version = "981d129";
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
                        configurePhase =
                            ''
                                export CFLAGS="$CFLAGS -ggdb ${asan_opt}"
                                export CXXFLAGS="$CXXFLAGS -ggdb ${asan_opt}"
                                export LDLAGS="$LDLAGS ${asan_opt}"
                                patchShebangs --build Configure
                                ./Configure --debug disable-tests -DPEDANTIC \
                                    -DFUZZING_BUILD_MODE_UNSAFE_FOR_PRODUCTION no-shared no-module \
                                    enable-tls1_3 enable-rc5 enable-md2 enable-ec_nistp_64_gcc_128 enable-ssl3 \
                                    enable-ssl3-method enable-nextprotoneg enable-weak-ssl-ciphers \
                                    ${asan_conf} $CFLAGS
                            '';
                        buildPhase =
                            ''
                                make -j$(nproc) clean
                                make -j$(nproc) LDCMD="$CXX $CXXFLAGS"
                                $CC $CFLAGS -ggdb -c -I${pinstrio_stubs}/include -DNO_LLVM -DPINSTRIO ${fuzzer_main}/fuzzer_main.c -o fuzzer_main.o
                                $CC $CFLAGS -ggdb -c -Iinclude -Ifuzz fuzz/fuzz_rand.c -o fuzz_rand.o
                                for fuzzer in asn1 asn1parse bignum server client x509; do \
                                    $CC $CFLAGS -ggdb -Iinclude -Ifuzz -c fuzz/$fuzzer.c -o $fuzzer.o; \
                                    $CXX $CXXFLAGS -ggdb -std=c++11 $fuzzer.o fuzz_rand.o fuzzer_main.o -o openssl_''${fuzzer}_current${asan_suff}.run ./libssl.a ./libcrypto.a $LDFLAGS $LIBS -L${pinstrio_stubs}/lib -lpinstrio; \
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
            name = "openssl_current_fuzzers";
            paths = (map (d: d.out) [(mk_fuzzers true) (mk_fuzzers false)]);
        }
