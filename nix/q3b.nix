{stdenv, lib, cmake, extra-cmake-modules, autoreconfHook, cudd, python39, jdk, antlr, z3}:
    let sources = import ./sources.nix {}; in
    let 
        libz3 =
            stdenv.mkDerivation
                {
                    pname = "libz3";
                    version = sources.z3.rev;
                    src = sources.z3;
                    nativeBuildInputs =
                        [
                            cmake
                            extra-cmake-modules
                            python39
                        ];
                    configurePhase =
                        ''
                            mkdir build
                            cd build
                            cmake -G "Unix Makefiles" -DZ3_BUILD_LIBZ3_SHARED=FALSE ..
                        '';
                    buildPhase =
                        ''
                            make -j$NIX_BUILD_CORES
                        '';
                    outputs = ["out" "lib" "include"];
                    installPhase =
                        ''
                            DESTDIR=$out make install
                            mv $out/var/empty/local/* $out
                            rm -r $out/var
                            mkdir -p $lib $include
                            for d in $out/lib*; do cp -a $d/. $lib; done
                            cp -a $out/include/. $include
                        '';
                }
        ; 
        antlr4_jar =
            stdenv.mkDerivation
                {
                    pname = "antlr4_jar";
                    version = sources.antlr4_jar.version;
                    src = sources.antlr4_jar;
                    phases = ["installPhase"];
                    installPhase =
                        ''
                            mkdir -p $out/bin
                            cp ${sources.antlr4_jar} $out/bin/antlr4-${sources.antlr4_jar.version}-complete.jar
                        '';
                }
        ; 
        antlr4 =
            stdenv.mkDerivation
                {
                    pname = "antlr4";
                    version = sources.antlr4.rev;
                    src = sources.antlr4;
                    nativeBuildInputs =
                        [
                            cmake
                            extra-cmake-modules
                        ];
                    configurePhase =
                        ''
                            export MAKEFLAGS=-j$NIX_BUILD_CORES
                            cd runtime/Cpp
                            mkdir build && mkdir run && cd build
                            cmake -DANTLR_BUILD_CPP_TESTS=OFF -DANTLR4_INSTALL=TRUE ..
                        '';
                    buildPhase =
                        ''
                            cmake --build . --target antlr4_static
                        '';
                    outputs = ["out" "lib" "include"];
                    installPhase =
                        ''
                            DESTDIR=$out make install
                            mv $out/var/empty/local/* $out
                            rm -r $out/var
                            mkdir -p $lib $include
                            cp -a $out/lib/. $lib
                            cp $out/lib/libantlr4-runtime.a $lib/libantlr4_static.a
                            cp -a $out/include/. $include
                        '';
                }
        ;
        parser =
            stdenv.mkDerivation
                {
                    pname = "parser";
                    version = sources.smtlibv2-grammar.rev;
                    src = sources.smtlibv2-grammar;
                    installPhase =
                        ''
                            mkdir -p $out
                            cp -r * $out
                        '';
                }
        ;
    in
    let 
        mk_q3b = {src, cudd, name, patches}:
            stdenv.mkDerivation
                {
                    pname = name;
                    version = src.rev;
                    src = src;
                    nativeBuildInputs =
                        [
                            cmake
                            extra-cmake-modules
                            jdk
                        ];
                    buildInputs =
                        [
                            cudd
                            libz3.lib
                            libz3.include
                            antlr4_jar
                            antlr4.lib
                            antlr4.include
                        ];
                    preConfigure =
                        ''
                            export CXXFLAGS="-I${libz3.include} -I${antlr4.include}/antlr4-runtime -L${antlr4.lib}"
                        '';
                    patches = patches;
                    postPatch =
                        ''
                            cp -r ${parser}/* parser/smtlibv2-grammar
                        '';
                    cmakeFlags = ["-DANTLR_EXECUTABLE=${antlr4_jar}/bin/antlr4-${antlr4_jar.version}-complete.jar"];
                    installPhase =
                        ''
                            mkdir -p $out/bin
                            cp q3b $out/bin/${name}
                        '';
                }
        ;
        cudd_3val =
            stdenv.mkDerivation
                {
                    pname = "cudd-3val";
                    version = sources.cudd-3val.rev;
                    src = sources.cudd-3val;
                    nativeBuildInputs = [autoreconfHook];
                    configureFlags =
                        [
                            "--enable-dddmp"
                            "--enable-obj"
                        ];
                }
        ;
    in
    {
        q3b = mk_q3b{src = sources.q3b; cudd = cudd; name = "q3b"; patches = [./q3b.diff ./q3b_bvcomp_fix.diff];};
        q3b_pbdd = mk_q3b{src = sources.q3b_pbdd; cudd = cudd_3val; name = "q3b_pbdd"; patches = [./q3b_pbdd.diff ./q3b_bvcomp_fix.diff];};
    }
