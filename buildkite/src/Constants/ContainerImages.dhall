-- TODO: Automatically push, tag, and update images #4862

{
  toolchainBase = "codaprotocol/ci-toolchain-base:v3",
  minaToolchainStretch = "gcr.io/o1labs-192920/mina-toolchain@sha256:7ef5827fa0854a0bcfdee69dcc0c2c7aef86e1662c630e71f07c1f9162e757fa",
  minaToolchainBuster = "gcr.io/o1labs-192920/mina-toolchain@sha256:71477097441872b96cce1ec6468c6aa3250bbc7159ecbcc9a716f5b54e16ec05",
  delegationBackendToolchain = "gcr.io/o1labs-192920/delegation-backend-production@sha256:8ca5880845514ef56a36bf766a0f9de96e6200d61b51f80d9f684a0ec9c031f4",
  elixirToolchain = "elixir:1.10-alpine",
  nodeToolchain = "node:14.13.1-stretch-slim",
  ubuntu1804 = "ubuntu:18.04",
  nix = "nixos/nix:2.6.0"
}
