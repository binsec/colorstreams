let
    pkgs = import (import ../../../nix/sources.nix {}).old_nixpkgs {};
    pinstrio_stubs = pkgs.callPackage ../../common/pinstrio_stubs.nix {};
in
pkgs.callPackage
    (
        {stdenv, autoconf, automake, autogen}:
            stdenv.mkDerivation
                {
                    name = "libsndfile";
                    src = pkgs.fetchFromGitHub
                        {
                            owner = "thorfdbg";
                            repo = "libjpeg";
                            rev = "db33a6ee2d";
                            sha256 = "sha256-cw/9qgWG8ZTNo5T3rRf0H/oUmr2H3DC9TEheTyJ42p4=";
                        };
                    version = "db33a6ee2d";
                    nativeBuildInputs =
                        [
                            autoconf
                            autogen
                            automake
                        ];
                    buildInputs =
                        [
                        ];
                    patches = [./patch.diff];
                    configurePhase =
                        ''
                            ./configure
                            export PINSTRIO_LIB="${pinstrio_stubs}/lib"
                            export PINSTRIO_INC="${pinstrio_stubs}/include"
                        '';
                    buildPhase =
                        ''
                            make
                        '';
                    installPhase =
                        ''
                            install -Dt $out/bin jpeg
                        '';
                    dontStrip = true;
                }
    ) {}
