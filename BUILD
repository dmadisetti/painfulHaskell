# Set all target’s visibility in this package to "public".
package(default_visibility = ["//visibility:public"])

# Load rules_haskell rules.
load(
    "@rules_haskell//haskell:defs.bzl",
    "haskell_toolchain_library",
)
load(
    "@euler//:euler.bzl",
    "euler_test",
)

# haskell_toolchain_library can access builtin GHC packages
# and assign them a bazel target name, so that they
# can be referenced as dependencies.
haskell_toolchain_library(name = "base")
