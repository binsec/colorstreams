let pkgs = import ./nix/pkgs.nix; in
pkgs.mkShell
    {
        packages = [(pkgs.python3.withPackages(p: with p; [matplotlib tabulate numpy]))];
        inputsFrom = [pkgs.colorstreams_final];
    }
