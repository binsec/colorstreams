let
    pkgs = import (import ../../../nix/sources.nix {}).old_nixpkgs {};
in
pkgs.callPackage
    (
        {stdenv}:
            stdenv.mkDerivation
                {
                    name = "sdop";
                    src = pkgs.fetchFromGitHub
                        {
                            owner = "PhilipHazel";
                            repo = "SDoP";
                            rev = "d6eda01";
                            sha256 = "sha256-fRYFMysqxZfikBzkypFvWpKVy+miOVRSDCCc/G25Vj8=";
                        };
                    version = "d6eda01";
                    preConfigure =
                    ''
                        export CC="gcc -g -fno-stack-protector -D_FORTIFY_SOURCE=0"
                    '';
                    dontStrip = true;
                }
    ) {}
