let
    pkgs = import (import ../../../nix/sources.nix {}).old_nixpkgs {};
    perl = pkgs.perl536.overrideAttrs
        (
            next: prev:
                {
                    preConfigure = 
                        ''
                            configureFlagsArray+=("-Dccflags=-fno-stack-protector -D_FORTIFY_SOURCE=0")
                            configureFlagsArray+=("-DEBUGGING=-g")
                            configureFlagsArray+=("-Dldflags=-lm")
                            configureFlagsArray+=("-Uusethreads")
                        '';
                    dontStrip = true;
                }
        );
    dbi = pkgs.stdenv.mkDerivation
        {
            name = "dbi";
            version = "1.642";
            src = pkgs.fetchurl
                {
                    url = "https://www.cpan.org/modules/by-module/DBI/DBI-1.642.tar.gz";
                    hash = "sha256-PyAlAjpWKGzr0Vy0leNszZtFbDzCKb8s4faenr/Cf10=";
                };
            nativeBuildInputs = [perl];
            configurePhase =
                ''
                    perl Makefile.PL INSTALL_BASE=$out
                '';
            postConfigure =
                ''
                    export NIX_CFLAGS_COMPILE="$NIX_CFLAGS_COMPILE -g -fno-stack-protector -D_FORTIFY_SOURCE=0"
                '';
            buildPhase =
                ''
                    make
                '';
            installPhase =
                ''
                    make install
                '';
            dontStrip = true;
        };
in
pkgs.mkShell
    {
        packages = [perl dbi];
        PERL5LIB = "${dbi}/lib/perl5/x86_64-linux";
        shellHook = "ln -s $(dirname $(which perl)) perl-dbi/result || true";
    }
