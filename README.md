# Colorstreams

A binary-level dynamic analysis platform with a focus on flexibility and composability.

## How to build and install

### Nix

*Colorstreams* is compiled through the *nix* package manager to ensure build reproducibility despite the large number of dependencies.

To install *nix*, follow [these instructions](https://nixos.org/download/).

Then build *Colorstreams*:

```bash
make build
```

And finally install it:

```bash
sudo make install
```

You can also open a shell with colorstreams and all its runtime dependencies:

```bash
nix-shell
```

Or package *Colorstreams* as an appimage:

```bash
make appimage
```

### Docker

To build the docker image:

```bash
docker build -t colorstreams .
```

To start a container:

```bash
docker run -it --rm colorstreams
```

## Usage

To learn how to use *Colorstreams*, please go through our [tutorials](./doc/tutorial/README.md).

For developers, have a look at the API documentation by running `make doc` after building and opening `doc/api/index.html` in your favorite browser.

## Benchmarks

We provide multiple benchmarks showcasing *Colorstreams'* capabilities.
Please refer to our [benchmark documentation](./test/README.md) for more details. 
