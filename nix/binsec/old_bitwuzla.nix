{stdenv, fetchurl, gmp, ocaml-ng}:
    let 
        ocaml = ocaml-ng.ocamlPackages; 
        sources = import ./../sources.nix {};
    in
    let bitwuzla-c = ocaml.buildDunePackage
            {
                pname = "bitwuzla-c";
                duneVersion = "3";
                #useDune2 = true;
                version = "1.0.5";
                src = sources.ocaml-bitwuzla-old;
                buildInputs = 
                    [
                        ocaml.zarith
                        gmp
                    ];
            };
    in
    let bitwuzla = ocaml.buildDunePackage
            {
                pname = "bitwuzla";
                duneVersion = "3";
                #useDune2 = true;
                version = "1.0.5";
                src = sources.ocaml-bitwuzla-old;
                propagatedBuildInputs = 
                    [
                        bitwuzla-c
                        ocaml.zarith
                        gmp
                    ];
            };
    in
{
    bitwuzla-c = bitwuzla-c;
    bitwuzla = bitwuzla;
}
