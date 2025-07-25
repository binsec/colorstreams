.PHONY: build install appimage doc

build:
	nix-build

install:
	cp result/bin/colorstreams /bin/colorstreams

doc:
	rm -rf doc/api && cp -r result/share/doc/_html doc/api && chmod -R ug+w doc/api

appimage:
	nix --extra-experimental-features "nix-command flakes" bundle --bundler github:ralismark/nix-appimage -f default.nix -o colorstreams
