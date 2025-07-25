let
    pkgs = import (import ../../nix/sources.nix {}).old_nixpkgs {};
    pinstrio_stubs = pkgs.callPackage ../common/pinstrio_stubs.nix {};
    fuzzer_main = pkgs.callPackage ../common/fuzzer_main.nix {};
    libxml2 = pkgs.callPackage ./nix/libxml2.nix {fuzzer_main = fuzzer_main; pinstrio_stubs = pinstrio_stubs;};
    poppler = pkgs.callPackage ./nix/poppler.nix {fuzzer_main = fuzzer_main; pinstrio_stubs = pinstrio_stubs;};
    openssl = pkgs.callPackage ./nix/openssl.nix {fuzzer_main = fuzzer_main; pinstrio_stubs = pinstrio_stubs;};
    #openssl_current = pkgs.callPackage ./nix/openssl_current.nix {fuzzer_main = fuzzer_main; pinstrio_stubs = pinstrio_stubs;};
    php = pkgs.callPackage ./nix/php.nix {fuzzer_main = fuzzer_main; pinstrio_stubs = pinstrio_stubs;};
    libtiff = pkgs.callPackage ./nix/libtiff.nix {fuzzer_main = fuzzer_main; pinstrio_stubs = pinstrio_stubs;};
    sqlite3 = pkgs.callPackage ./nix/sqlite3.nix {fuzzer_main = fuzzer_main; pinstrio_stubs = pinstrio_stubs;};
    libpng = pkgs.callPackage ./nix/libpng.nix {fuzzer_main = fuzzer_main; pinstrio_stubs = pinstrio_stubs;};

in
pkgs.symlinkJoin
    {
        name = "magma_fuzzers";
        paths = (map (d: d.out) [libxml2 poppler openssl libtiff sqlite3 libpng php]);
    }
