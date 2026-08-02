FROM debian:stable-slim

RUN apt-get update && apt-get install -y --no-install-recommends \
    ghc cabal-install zlib1g-dev g++ ca-certificates \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app
COPY . /app
RUN cabal update \
    && cabal install --installdir=/out --install-method=copy --overwrite-policy=always

# build + extract:
#   podman build -t leu-build .
#   podman create --name leu-tmp leu-build
#   podman cp leu-tmp:/out/leu ./leu
#   podman rm leu-tmp
