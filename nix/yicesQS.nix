{stdenv, ocaml-ng, yices, gmp}:
    let sources = import ./sources.nix {}; in
    let ctypes_zarith =
        ocaml-ng.ocamlPackages.buildDunePackage
            {
                pname = "ctypes-zarith";
                version = sources.ctypes-zarith.rev;
                src = sources.ctypes-zarith;
                nativeBuildInputs =
                    [
                        ocaml-ng.ocamlPackages.ppx_cstubs
                    ];
                propagatedBuildInputs =
                    [
                        gmp
                        ocaml-ng.ocamlPackages.dune-configurator
                        ocaml-ng.ocamlPackages.ctypes
                        ocaml-ng.ocamlPackages.zarith
                        ocaml-ng.ocamlPackages.ppx_cstubs
                    ];
            }
        ;
    in
    let yices_ocaml =
        ocaml-ng.ocamlPackages.buildDunePackage
            {
                pname = "yices2_bindings";
                version = sources.yices2_ocaml_bindings.rev;
                src = sources.yices2_ocaml_bindings;
                propagatedBuildInputs =
                    [
                        ctypes_zarith
                        ocaml-ng.ocamlPackages.containers
                        ocaml-ng.ocamlPackages.ppx_deriving
                        ocaml-ng.ocamlPackages.ppx_optcomp
                        ocaml-ng.ocamlPackages.sexplib
                        ocaml-ng.ocamlPackages.sexplib0
                        yices
                    ];
                buildPhase =
                    ''
                        make
                    '';
            }
        ;
    in
    ocaml-ng.ocamlPackages.buildDunePackage
        {
            pname = "yicesQS";
            version = sources.yicesQS.rev; 
            src = sources.yicesQS;
            buildInputs =
                [
                    yices_ocaml
                ];
            patches = [./yicesQS.diff];
            buildPhase =
                ''
                    make
                '';
            installPhase =
                ''
                    mkdir -p $out/bin
                    mv main.exe $out/bin/yicesQS
                '';
        }
