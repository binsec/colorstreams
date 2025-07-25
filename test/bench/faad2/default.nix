let
    pkgs = import (import ../../../nix/sources.nix {}).old_nixpkgs {};
in
pkgs.faad2.overrideAttrs
    (
        next: prev:
            {
                version = "bfab0b0";
                src = pkgs.fetchFromGitHub
                    {
                        owner = "knik0";
                        repo = "faad2";
                        rev = next.version;
                        sha256 = "iZg42U3C15EN5rNgAStdfc3VvrItai+7pY5BPjH/YFA=";
                    };
                preConfigure =
                    ''
                        export CC="gcc -g -fno-stack-protector -D_FORTIFY_SOURCE=0"
                    '';
                dontStrip = true;
            }
    )
