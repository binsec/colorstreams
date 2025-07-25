{stdenv, ocaml-ng, nix-gitignore, binsec}:
    let ocaml = ocaml-ng.ocamlPackages; in
    ocaml.buildDunePackage
        {
            pname = "colorstreams";
            duneVersion = "3";
            version = builtins.elemAt (builtins.match ".*version \"([^\"]*)\".*" (builtins.readFile ../dune-project)) 0;
            src = nix-gitignore.gitignoreSource ["nix" "test" "modules" "doc"] ./..;
            nativeBuildInputs =
                [
                    ocaml.odoc
                ];
            propagatedBuildInputs =
                [
                    ocaml.yojson
                    binsec
                ];
            postBuild =
                ''
                    dune build @doc
                '';
            postInstall =
                ''
                    cp -r _build/default/_doc $out/doc
                '';
        }
