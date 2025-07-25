let
    pkgs = import (import ../../../nix/sources.nix {}).old_nixpkgs {};
    wd = ./.;
in
pkgs.stdenv.mkDerivation
    {
        name = "mjs";
        src = pkgs.fetchFromGitHub
            {
                owner = "cesanta";
                repo = "mjs";
                rev = "2.20.0";
                sha256 = "FBMoP28942Bwx0zFISBPYvH6jvXqLFmvDXHkxLHBCjY=";
            };
        version = "2.20.0";
        nativeBuildInputs = [pkgs.patch];
        configurePhase =
            ''
                sed -i -e "s/double /int64_t /g" mjs.c mjs.h
                sed -i -e "s/(double)/(int64_t)/g" mjs.c mjs.h
                sed -i -e "s/strtod/strtod_stub/g" mjs.c mjs.h
                sed -i -e "s/isnan/isnan_stub/g" mjs.c mjs.h
                #pointers and numbers can no longer be distinguished
                sed -i -e "s/da = mjs_is_number(a) ?/da = 0 ?/g" mjs.c
                echo "#include \"stdlib.h\"" >> mjs.h
                echo "int64_t strtod_stub(char *s, char **end){return strtoll(s, end, 10);}" >> mjs.h
                echo "int isnan_stub(int64_t i){return 0;}" >> mjs.h
                patch -p1 -i ${wd}/stubs.diff
            '';
        buildPhase =
            ''
                gcc -g -o mjs.run mjs.c -DMJS_MAIN
            '';
        installPhase =
            ''
                install -Dt $out mjs.run
                #cp mjs.c $out/mjs.c
            '';
        dontStrip = true;
    }
