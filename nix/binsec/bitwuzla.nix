{stdenv, fetchurl, gmp, ocaml-ng}:
    let 
        sources = import ../sources.nix {}; 
        ocaml = ocaml-ng.ocamlPackages;
    in
    ocaml.buildDunePackage
        {
            pname = "bitwuzla-cxx";
            version = sources.ocaml-bitwuzla.rev;
            src = sources.ocaml-bitwuzla;
            duneVersion = "3";
            buildInputs = 
                [
                    ocaml.zarith
                    gmp
                ];
        }
