{stdenv, bitwuzla, old_bitwuzla}:
    stdenv.mkDerivation
        {
            pname = "bitwuzla-switch";
            version = "1.0";
            src = ./bitwuzla_switch;

            installPhase =
                ''
                    install -Dt $out/bin bitwuzla
                    cp ${old_bitwuzla}/bin/bitwuzla $out/bin/old_bitwuzla
                    cp ${bitwuzla}/bin/bitwuzla $out/bin/new_bitwuzla
                '';
        }
