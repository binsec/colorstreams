{stdenv, coreutils, ocaml-ng, makeWrapper, lib, colorstreams, pin, pinstrio, bitwuzla_switch, binsec, popcon, boolector, ganak, approxmc, z3, q3b, q3b_pbdd, gdb}:
    let
        sources = import ./sources.nix {};
        gdb_12 = (import sources.nixpkgs_gdb_12 {}).gdb;
    in
    stdenv.mkDerivation
        {
            pname = "colorstreams";
            version = colorstreams.version;
            src = colorstreams;
            nativeBuildInputs =
                [
                    makeWrapper
                ];
            propagatedBuildInputs =
                [
                    ganak
                    approxmc
                    z3
                    popcon
                    boolector
                    bitwuzla_switch
                    q3b
                    q3b_pbdd
                    pin
                    pinstrio
                    gdb_12
                    colorstreams
                ];
            installPhase =
                ''
                    install -Dt $out/bin ${colorstreams}/bin/colorstreams
                    cp -r ${colorstreams}/doc $out/doc
                '';
            postFixup =
                ''
                    wrapProgram $out/bin/colorstreams \
                    --prefix PATH : ${lib.makeBinPath
                        [
                            ganak
                            approxmc
                            z3
                            boolector
                            bitwuzla_switch
                            q3b
                            q3b_pbdd
                            popcon
                            pin
                            pinstrio
                            gdb_12
                        ]} \
                    --set PINSTRIO ${lib.makeBinPath [pinstrio]}/pinstrio.so
                '';
        }
