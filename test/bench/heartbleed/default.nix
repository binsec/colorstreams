let
    nixpkgs_impure = import <nixpkgs> {};
    nixpkgs = nixpkgs_impure.fetchFromGitHub 
        {
            owner = "nixos";
            repo = "nixpkgs";
            rev = "5f506b95f9f"; # 19.09 in november
            sha256 = "14ry2fs2pny2shlg2dq8c9vns636gv0zvfh0dmyy4bnq713k377n";
        };
    lib = import (nixpkgs + "/lib");
    pkgs = import nixpkgs {};
    pinstrio_stubs = pkgs.callPackage ../../common/pinstrio_stubs.nix {};
in
pkgs.callPackage 
    (
        {buildPackages, stdenv, perl}:
            let libpinstrio = ../libpinstrio.a; in
            stdenv.mkDerivation 
                {
                    name = "heartbleed";
                    src = fetchGit
                        {
                            url = "https://github.com/openssl/openssl.git";
                            rev = "0d8776344cd7965fadd870e361503985df9c45bb";
                            ref = "OpenSSL_1_0_1-stable";
                        };
                    patches = [./patch.diff];
                    nativeBuildInputs = 
                        [ 
                            perl
                        ];
                    configurePhase = 
                        ''
                            ./config -d -DPINSTRIO_COMMON -L${pinstrio_stubs}/lib -lpinstrio -no-threads
                        '';
                    installPhase = 
                        ''
                            install -Dt $out apps/openssl
                        '';
                }
    ) {}
