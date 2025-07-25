let 
    pkgs = import (import ../../nix/sources.nix {}).nixpkgs {}; 
    pinstrio_stubs = pkgs.callPackage ../../test/common/pinstrio_stubs.nix {};
in
pkgs.mkShell
    {
        PINSTRIO_LIB = "${pinstrio_stubs}/lib";
        buildInputs = [pinstrio_stubs];
    }
