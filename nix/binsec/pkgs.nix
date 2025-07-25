let 
    sources = import ../sources.nix {};
    overlay =
        self: super:
            {
                unisim_archisec = super.callPackage ./unisim_archisec.nix {};
                inherit (super.callPackage ./old_bitwuzla.nix {}) bitwuzla;
                bitwuzla-cxx = super.callPackage ./bitwuzla.nix {};
                binsec = (self.callPackage ./binsec.nix {}).overrideAttrs(prev: {doCheck = false;});
            };
in
import sources.nixpkgs {overlays = [overlay];}
