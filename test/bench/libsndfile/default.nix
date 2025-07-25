let
    pkgs = import (import ../../../nix/sources.nix {}).old_nixpkgs {};
    pinstrio_stubs = pkgs.callPackage ../../common/pinstrio_stubs.nix {};
in
pkgs.callPackage
    (
        {stdenv, cmake, extra-cmake-modules, autoconf, autogen, automake, flac, libogg, libtool, libvorbis, libopus, pkg-config, python39}:
            let
                vuln =
                    stdenv.mkDerivation
                        {
                            name = "libsndfile";
                            src = pkgs.fetchFromGitHub
                                {
                                    owner = "libsndfile";
                                    repo = "libsndfile";
                                    rev = "v1.0.30";
                                    sha256 = "sha256-irMHlzxzuHCqUcw/4aJXt10Zp9C/QjVFZVxUoolPB+Y=";
                                };
                            version = "1.0.30";
                            nativeBuildInputs =
                                [
                                    cmake
                                    extra-cmake-modules
                                    autoconf
                                    autogen
                                    automake
                                    pkg-config
                                    python39
                                ];
                            buildInputs =
                                [
                                    flac
                                    libogg
                                    libtool
                                    libvorbis
                                    libopus
                                ];
                            patches = [./patch.diff];
                            configurePhase =
                                ''
        #                            export CC="gcc -fsanitize=address"
                                    export CFLAGS="-g"
                                    export LDFLAGS="-L${pinstrio_stubs}/lib"
                                    export LIBS="-lpinstrio"
                                    ./autogen.sh
                                    ./configure --disable-shared
                                '';
                            buildPhase =
                                ''
                                    make
                                '';
                            installPhase =
                                ''
                                    install -Dt $out/bin programs/sndfile-info
                                '';
                            dontStrip = true;
                        };
                in
                [vuln]
    ) {}
