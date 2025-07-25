SRC = $(wildcard *.c)
TODO = $(SRC:c=run)

.PHONY: build clean

build: $(TODO)

clean:
	rm -f *.run

%.run:
	nix-shell --run "gcc -g -fno-stack-protector -O0 -o $@ $(@:run=c) -lpinstrio" ./shell.nix
