let
    pkgs = import (import ../../../nix/sources.nix {}).old_nixpkgs {};
in
pkgs.callPackage
    (
        {stdenv}:
            stdenv.mkDerivation
                {
                    name = "xfpt";
                    src = pkgs.fetchFromGitHub
                        {
                            owner = "PhilipHazel";
                            repo = "xfpt";
                            rev = "84cc672";
                            sha256 = "e8welt0rOEyA6dQZE0N9pVWkAG3Vh1nOYBV42fUuVx8=";
                        };
                    version = "84cc672";
                    preConfigure =
                    ''
                        export CC="gcc -g -fno-stack-protector -D_FORTIFY_SOURCE=0"
                    '';
                    dontStrip = true;
                }
    ) {}
