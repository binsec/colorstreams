{stdenv}:
    stdenv.mkDerivation
        {
            name = "fuzzer_main";
            src = ./.;
            version = "1";
            installPhase = 
                ''
                    install -Dt $out fuzzer_main.c
                '';
        }
