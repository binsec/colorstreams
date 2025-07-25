{stdenv}:
    stdenv.mkDerivation
        {
            pname = "pinstrio_stubs";
            version = "local";
            src = ../../tracing/pinstrio;
            preBuild =
                ''
                    cd stubs
                '';
            installPhase =
                ''
                    install -Dt $out/lib libpinstrio.a
                    install -Dt $out/include pinstrio.h
                '';
        }
