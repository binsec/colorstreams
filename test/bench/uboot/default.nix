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
    pkgs = import nixpkgs 
        {
#            crossSystem = lib.systems.examples.armv7l-hf-multiplatform;
        };
    pinstrio_stubs = pkgs.callPackage ../../common/pinstrio_stubs.nix {};
in
pkgs.callPackage 
    (
        {buildPackages, stdenv, openssl, SDL, SDL2, flex, ncurses, pkg-config, bison, yacc, perl}:
            let 
                mySDL = SDL.override 
                    {
                        libGLSupported = false;
                        openglSupport = false;
                        libGL = null;
                        libGLU = null;
                        alsaSupport  = false;
                        alsaLib = null;
                        x11Support  = false;
                        libXext = null;
                        libICE = null;
                        libXrandr = null;
                        pulseaudioSupport  = false;
                        libpulseaudio = false;
                    };
                mySDL2 = SDL2.override 
                    {
                        libGLSupported = false;
                        openglSupport = false;
                        libGL = null;
#                        libGLU = null;
                        alsaSupport  = false;
                        alsaLib = null;
                        x11Support  = false;
                        libXext = null;
                        libICE = null;
                        libXrandr = null;
                        pulseaudioSupport  = false;
                        libpulseaudio = false;
                    };
            in
            let 
                v2019_07 =
                    stdenv.mkDerivation 
                        {
                            name = "uboot_2019_07";
                            src = nixpkgs_impure.fetchFromGitHub
                                {
                                    owner = "u-boot";
                                    repo = "u-boot";
                                    rev = "v2019.07";
                                    sha256 = "sha256-7LArjKEKQLIGKVou83NapojPiFzsTUinUMQXaJkwxKc=";
                                };
                            version = "v2019.07";
                            nativeBuildInputs = 
                                [ 
                                    buildPackages.stdenv.cc 
                                    bison 
                                    yacc 
                                    flex 
                                ];
                            depsBuildBuild = 
                                [ 
                                    ncurses 
                                    pkg-config 
                                ]; # for make menuconfig
                            buildInputs = 
                                [ 
                                    mySDL 
                                    openssl
                                ];
                            patches = [./patch.diff];
                            configurePhase = 
                                ''
                                    make sandbox_defconfig CONFIG_SANDBOX_32BIT=0
                                '';
                            installPhase = 
                                ''
                                    install -Dt $out u-boot
                                    install -Dt $out u-boot.dtb
        #                            mkdir $out/src
        #                            cp -r . $out/src
                                '';
                            dontStrip = true;
                        };
                v2019_07_noabort =
                    stdenv.mkDerivation 
                        {
                            name = "uboot_2019_07";
                            src = nixpkgs_impure.fetchFromGitHub
                                {
                                    owner = "u-boot";
                                    repo = "u-boot";
                                    rev = "v2019.07";
                                    sha256 = "sha256-7LArjKEKQLIGKVou83NapojPiFzsTUinUMQXaJkwxKc=";
                                };
                            version = "v2019.07";
                            nativeBuildInputs = 
                                [ 
                                    buildPackages.stdenv.cc 
                                    bison 
                                    yacc 
                                    flex 
                                ];
                            depsBuildBuild = 
                                [ 
                                    ncurses 
                                    pkg-config 
                                ]; # for make menuconfig
                            buildInputs = 
                                [ 
                                    mySDL 
                                    openssl
                                ];
                            patches = [./patch_no_abort.diff];
                            configurePhase = 
                                ''
                                    make sandbox_defconfig CONFIG_SANDBOX_32BIT=0
                                '';
                            installPhase = 
                                ''
                                    install -Dt $out u-boot
                                    install -Dt $out u-boot.dtb
        #                            mkdir $out/src
        #                            cp -r . $out/src
                                '';
                            dontStrip = true;
                        };
                v2022_01 =
                    stdenv.mkDerivation 
                        {
                            name = "uboot_2022_01";
                            src = nixpkgs_impure.fetchFromGitHub
                                {
                                    owner = "u-boot";
                                    repo = "u-boot";
                                    rev = "v2022.01";
                                    sha256 = "sha256-kKxo62/TI0HD8uZaL39FyJc783JsErkfspKsQ6uvEMU=";
                                };
                            version = "v2022.01";
                            nativeBuildInputs = 
                                [ 
                                    buildPackages.stdenv.cc 
                                    bison 
                                    yacc 
                                    flex 
                                    perl
                                ];
                            depsBuildBuild = 
                                [ 
                                    ncurses 
                                    pkg-config 
                                ]; # for make menuconfig
                            buildInputs = 
                                [ 
                                    mySDL2
                                    openssl
                                    pinstrio_stubs
                                ];
                            patches = [./patch2.diff];
                            configurePhase = 
                                ''
                                    patchShebangs .
                                    export PINSTRIO_LIB="${pinstrio_stubs}/lib"
                                    make sandbox_defconfig CONFIG_SANDBOX_32BIT=0
                                '';
                            installPhase = 
                                ''
                                    install -Dt $out u-boot
                                    install -Dt $out u-boot.dtb
        #                            mkdir $out/src
        #                            cp -r . $out/src
                                '';
                            dontStrip = true;
                        };
            in
            [v2019_07 v2022_01 v2019_07_noabort]
    ) {}
