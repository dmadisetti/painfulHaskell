# [Choice] Ubuntu version: bionic, focal
ARG VARIANT="focal"
FROM mcr.microsoft.com/vscode/devcontainers/base:0-${VARIANT}

RUN apt-get update && export DEBIAN_FRONTEND=noninteractive \
    && apt-get -y install --no-install-recommends \
    python python3 libgmp-dev npm clang-11 make libncurses5 \
    libblas-dev liblapack-dev

RUN npm install -g @bazel/bazelisk
ENV USE_BAZEL_VERSION=2.1.0
ENV CC=clang-11