let
    sources = import ./sources.nix {};
    old_nixpkgs = import sources.old_nixpkgs {};
in
import sources.nixpkgs
    {
        overlays =
            [
                (
                    self: super: 
                        {
                            pin = old_nixpkgs.callPackage ./pin.nix {};
                            pinstrio = old_nixpkgs.callPackage ./pinstrio.nix {pin = self.pin;};
                            ganak = super.callPackage ./ganak.nix {};
                            popcon = (import (sources.popcon.outPath + "/nix/pkgs.nix")).popcon;
                            inherit (old_nixpkgs.callPackage ./q3b.nix {}) q3b q3b_pbdd;
                            #yicesQS = super.callPackage ./yicesQS.nix {};
                            inherit (import ./binsec/pkgs.nix) binsec;
                            old_bitwuzla = old_nixpkgs.bitwuzla;
                            bitwuzla_switch = super.callPackage ./bitwuzla_switch.nix {};
                            colorstreams = super.callPackage ./colorstreams.nix {};
                            colorstreams_final = super.callPackage ./colorstreams_final.nix {};
                        }
                )
            ];
    }
