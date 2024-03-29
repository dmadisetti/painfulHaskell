workspace(name = "painful_haskell")

# Load the repository rule to download an http archive.
load(
    "@bazel_tools//tools/build_defs/repo:http.bzl",
    "http_archive",
)

# Download rules_haskell and make it accessible as "@rules_haskell".
# Update to head
http_archive(
    name = "rules_haskell",
    strip_prefix = "rules_haskell-57081449c34c0a3de429759837f61a0b70a8f3e2",
    urls = ["https://github.com/tweag/rules_haskell/archive/57081449c34c0a3de429759837f61a0b70a8f3e2.tar.gz"],
    sha256 = "e258692e7e4266c71ad8ef77c8179a8dba179f0854121008827189764f571625",
)

load(
    "@rules_haskell//haskell:repositories.bzl",
    "rules_haskell_dependencies",
)

# Setup all Bazel dependencies required by rules_haskell.
rules_haskell_dependencies()

load(
    "@rules_haskell//haskell:toolchain.bzl",
    "rules_haskell_toolchains",
)

# Download a GHC binary distribution from haskell.org and register it as a toolchain.
rules_haskell_toolchains()

load("@rules_haskell//haskell:cabal.bzl", "stack_snapshot")

stack_snapshot(
    name = "stackage",
    packages = ["hashmap", "hashable", "hlint", "numbers", "containers", "sort", "split", "hmatrix"],
    components = {
        "hlint": ["exe"],
        "containers": ["lib"],
    },
    snapshot = "lts-18.26",
)

http_archive(
    name = "euler",
    strip_prefix = "rules_euler-master",
    urls = ["https://github.com/dmadisetti/rules_euler/archive/master.tar.gz"],
    patches = ["euler.patch"],
    sha256 = "40ad73ea6917adfa739f503689cdc3a10be72e9487821a97bd8b7874bbaf42fa",
    patch_cmds = ["chmod +x stub/stub.sh"]
)

load("@euler//:euler.bzl", "euler_repositories", "euler_test")

euler_repositories()
