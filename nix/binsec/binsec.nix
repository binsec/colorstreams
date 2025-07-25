{ stdenv
, lib
, nix-gitignore
, ocaml-ng
, gmp
, unisim_archisec
, bubblewrap
, glibc
, pkgsi686Linux
, python3
, bitwuzla
, boolector
, z3
, gdb
, file
, bitwuzla-cxx
}:
    let 
        sources = import ../sources.nix {}; 
        ocaml = ocaml-ng.ocamlPackages;
    in
    ocaml.buildDunePackage
        {
            pname = "binsec";
            version = sources.binsec.rev;
            src = sources.binsec;
            duneVersion = "3";

            nativeBuildInputs = 
                [
                    ocaml.dune-site
                    ocaml.menhir
                    ocaml.dypgen
                ];

            propagatedBuildInputs = 
                [
                    gmp # for zarith
                    ocaml.ocamlgraph
                    ocaml.zarith
                    #ocaml.llvm does not work
                    ocaml.dune-site
                    ocaml.menhir
                    ocaml.toml
                    unisim_archisec
                    #ocaml.curses does not work
                    ocaml.mmap
                    bitwuzla
                    bitwuzla-cxx
                    ocaml.dypgen
                    ocaml.findlib #necessary
                ];

            checkInputs = 
                [
                    ocaml.qtest
                    ocaml.ounit
                    ocaml.qcheck
                    ocaml.seq
                    bubblewrap
                    python3
                    z3
                    bitwuzla
                    boolector
                    gdb
                    file
                ];

            doCheck = true;
            checkPhase = 
                ''
                    runHook preCheck
                    if [ -z "$(ls examples)" ]; then
                      echo "this build does not have the submodule in examples, not all tests will be run"
                    fi
                    patchShebangs --build .
                    # binsec-examples contains executables that depend on /lib/ld-linux.so.2
                    # really existing, which is not the case in the nix sandbox. Provide them
                    # manually with bwrap
                    bwrap_args=()
                    for i in /*; do
                      case "$i" in
                      /dev)
                    bwrap_args+=(--dev /dev)
                    ;;
                      *)
                    bwrap_args+=(--bind $i $i)
                    ;;
                      esac
                    done
                    bwrap_args+=(--ro-bind ${glibc}/lib64/ld-linux-x86-64.so.2 /lib64/ld-linux-x86-64.so.2 --ro-bind ${pkgsi686Linux.glibc}/lib/ld-linux.so.2 /lib/ld-linux.so.2)
                    bwrap "''${bwrap_args[@]}" dune test -j$NIX_BUILD_CORES
                    runHook postCheck
                '';

}
