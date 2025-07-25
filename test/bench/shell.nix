let 
    pkgs = import (import ../../nix/sources.nix {}).old_nixpkgs {}; 
    pinstrio_stubs = pkgs.callPackage ../common/pinstrio_stubs.nix {};
in
pkgs.mkShell
    {
        PINSTRIO_LIB = "${pinstrio_stubs}/lib";
        buildInputs = [pinstrio_stubs];
    }
