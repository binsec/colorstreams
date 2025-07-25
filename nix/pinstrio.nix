{stdenv, gnat11, pin}:
    stdenv.mkDerivation
        {
            pname = "pinstrio";
            version = "local";
            src = ../tracing/pinstrio;
            nativeBuildInputs =
                [
                    gnat11
                ];
            buildInputs = 
                [
                    pin
                ];
            PIN_ROOT = pin + "/bin";
            buildPhase =
                ''
                    rm -rf obj-intel64
                    make obj-intel64/pinstrio.so CXX+="-fno-rtti -fno-exceptions"
                    cd stubs
                    make
                    cd ..
                '';
            installPhase =
                ''
                    install -Dt $out/bin obj-intel64/pinstrio.so
                    install -Dt $out/lib stubs/libpinstrio.a
                '';
        }
