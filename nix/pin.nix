{stdenv}:
    let sources = import ./sources.nix {}; in
    stdenv.mkDerivation 
        {
            pname = "pin";
            version = "3.23";
            src = sources.pin;
            installPhase =
                ''
                    mkdir -p $out/bin
                    cp -r * $out/bin
                '';
        }
