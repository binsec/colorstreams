{stdenv, lib, git, ocaml-ng}:
    let 
        sources = import ../sources.nix {}; 
        ocaml = ocaml-ng.ocamlPackages;
    in
    ocaml.buildDunePackage
        {
            pname = "unisim_archisec";
            version = sources.unisim_archisec.rev;
            src = sources.unisim_archisec;
            nativeBuildInputs = 
                [
                    ocaml.odoc 
                    git
                ];
        }
