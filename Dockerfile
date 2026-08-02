FROM debian:stable-slim

RUN apt-get update && apt-get install -y --no-install-recommends \
    gcc libc6-dev libcurl4-openssl-dev libxml2-dev pkg-config ca-certificates \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app
COPY . /app
RUN ./build.sh && mkdir -p /out && cp build/leu /out/leu

# build + extract:
#   podman build -t leu-build .
#   podman create --name leu-tmp leu-build
#   podman cp leu-tmp:/out/leu ./leu
#   podman rm leu-tmp
