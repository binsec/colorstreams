{stdenv, fetchzip}:
    let sources = import ./sources.nix {}; in
    stdenv.mkDerivation   
        {
            pname = "ganak";
            version = "2.4.3";
            src = fetchzip
                {
                    url = "https://github.com/meelgroup/ganak/releases/download/release%2F2.4.3/ganak-linux-amd64.zip";
                    hash = "sha256-yvv0bQqr3AIT+ewibUCsQRk2SvFzfSklqVFq9RVOZ5Y=";
                };
            buildPhase = "";
            installPhase = "install -Dt $out/bin ganak";
        }
