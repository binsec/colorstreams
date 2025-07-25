#note: pin does not work with nix's official docker image
FROM ubuntu:jammy

RUN apt update
RUN apt install -y curl vim xz-utils make git

RUN curl -L https://nixos.org/nix/install > nix.sh
RUN chmod +x nix.sh
RUN ./nix.sh --daemon
RUN echo ". $HOME/.nix-profile/etc/profile.d/nix.sh" >> ~/.bashrc
ENV PATH="$PATH:/root/.nix-profile/bin"

ADD . /colorstreams
WORKDIR /colorstreams
RUN nix-shell --command "echo"

WORKDIR test/magma
RUN make build

WORKDIR ../bench
RUN make build

WORKDIR ../..

CMD nix-shell
