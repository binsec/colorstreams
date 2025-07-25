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
        {stdenv, lib}: 
            stdenv.mkDerivation 
                {
                    name = "mongoose";
                    src = fetchTarball 
                        {
                            url = "https://github.com/cesanta/mongoose/archive/6.16.tar.gz";
                            sha256 = "1pi59h6dwsibmpi6pqgw2vdi7bncplk5smxiiqaf0y09kvj3fpai";
                        };
                    buildInput = [pinstrio_stubs];
                    patches = [./patch.diff];
                    buildPhase =
                        ''
                            cd examples/mqtt_broker/
                            make ADDITIONAL_OPTS="-I${pinstrio_stubs}/include -L${pinstrio_stubs}/lib"
                        '';
                    installPhase = 
                        ''
                            install -Dt $out/bin mqtt_broker
                        '';
                    dontStrip = true;
                }
    ) {}
