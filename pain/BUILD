# Set all target’s visibility in this package to "public".
package(default_visibility = ["//visibility:public"])

# Load rules_haskell rules.
load(
    "@rules_haskell//haskell:defs.bzl",
    "haskell_binary",
    "haskell_library",
)
load(
    "@euler//:euler.bzl",
    "euler_test",
)

haskell_library(
    name = "helpers",
    srcs = ["helpers.hs"],
    deps = ["//:base"],
)

haskell_binary(
    name = "euler_bin1",
    srcs = ["euler_1.hs"],
    deps = ["//:base"],
)

euler_test(
    name = "euler_1_test",
    problem = 1,
    solution = ":euler_bin1",
)

haskell_binary(
    name = "euler_bin2",
    srcs = ["euler_2.hs"],
    deps = ["//:base"],
)

euler_test(
    name = "euler_2_test",
    problem = 2,
    solution = ":euler_bin2",
)

haskell_binary(
    name = "euler_bin3",
    srcs = ["euler_3.hs"],
    deps = ["//:base", ":helpers"],
)

euler_test(
    name = "euler_3_test",
    problem = 3,
    solution = ":euler_bin3",
)

haskell_binary(
    name = "euler_bin4",
    srcs = ["euler_4.hs"],
    deps = ["//:base", "helpers"],
)

euler_test(
    name = "euler_4_test",
    problem = 4,
    solution = ":euler_bin4",
)

haskell_binary(
    name = "euler_bin5",
    srcs = ["euler_5.hs"],
    deps = ["//:base", "helpers"],
)

euler_test(
    name = "euler_5_test",
    problem = 5,
    solution = ":euler_bin5",
)

haskell_binary(
    name = "euler_bin6",
    srcs = ["euler_6.hs"],
    deps = ["//:base", "helpers"],
)

euler_test(
    name = "euler_6_test",
    problem = 6,
    solution = ":euler_bin6",
)

haskell_binary(
    name = "euler_bin7",
    srcs = ["euler_7.hs"],
    deps = ["//:base", "helpers"],
)

euler_test(
    name = "euler_7_test",
    problem = 7,
    solution = ":euler_bin7",
)

haskell_binary(
    name = "euler_bin8",
    srcs = ["euler_8.hs"],
    deps = ["//:base"],
)

euler_test(
    name = "euler_8_test",
    problem = 8,
    solution = ":euler_bin8",
)

haskell_binary(
    name = "euler_bin9",
    srcs = ["euler_9.hs"],
    deps = ["//:base"],
)

euler_test(
    name = "euler_9_test",
    problem = 9,
    solution = ":euler_bin9",
)

haskell_binary(
    name = "euler_bin10",
    srcs = ["euler_10.hs"],
    deps = ["//:base", "helpers"],
)

euler_test(
    name = "euler_10_test",
    problem = 10,
    solution = ":euler_bin10",
)

haskell_binary(
    name = "euler_bin11",
    srcs = ["euler_11.hs"],
    deps = ["//:base"],
)

euler_test(
    name = "euler_11_test",
    problem = 11,
    solution = ":euler_bin11",
)

haskell_binary(
    name = "euler_bin12",
    srcs = ["euler_12.hs"],
    deps = ["@stackage//:hashmap", "//:base", "helpers"],
)

euler_test(
    name = "euler_12_test",
    problem = 12,
    solution = ":euler_bin12",
)

haskell_binary(
    name = "euler_bin13",
    srcs = ["euler_13.hs"],
    deps = ["//:base"],
)

euler_test(
    name = "euler_13_test",
    problem = 13,
    solution = ":euler_bin13",
)

haskell_binary(
    name = "euler_bin14",
    srcs = ["euler_14.hs"],
    deps = ["//:base"],
)

euler_test(
    name = "euler_14_test",
    problem = 14,
    solution = ":euler_bin14",
)

haskell_binary(
    name = "euler_bin15",
    srcs = ["euler_15.hs"],
    deps = ["//:base", ":helpers"],
)

euler_test(
    name = "euler_15_test",
    problem = 15,
    solution = ":euler_bin15",
)

haskell_binary(
    name = "euler_bin16",
    srcs = ["euler_16.hs"],
    deps = ["//:base"],
)

euler_test(
    name = "euler_16_test",
    problem = 16,
    solution = ":euler_bin16",
)

haskell_binary(
    name = "euler_bin17",
    srcs = ["euler_17.hs"],
    deps = ["//:base"],
)

euler_test(
    name = "euler_17_test",
    problem = 17,
    solution = ":euler_bin17",
)

haskell_binary(
    name = "euler_bin18",
    srcs = ["euler_18.hs"],
    deps = ["//:base"],
)

euler_test(
    name = "euler_18_test",
    problem = 18,
    solution = ":euler_bin18",
)

haskell_binary(
    name = "euler_bin28",
    srcs = ["euler_28.hs"],
    deps = ["//:base", "helpers"],
)

euler_test(
    name = "euler_28_test",
    problem = 28,
    solution = ":euler_bin28",
)

haskell_binary(
    name = "euler_bin20",
    srcs = ["euler_20.hs"],
    deps = ["//:base", ":helpers"],
)

euler_test(
    name = "euler_20_test",
    problem = 20,
    solution = ":euler_bin20",
)

haskell_binary(
    name = "euler_bin21",
    srcs = ["euler_21.hs"],
    deps = ["//:base", ":helpers"],
)

euler_test(
    name = "euler_21_test",
    problem = 21,
    solution = ":euler_bin21",
)

haskell_binary(
    name = "euler_bin35",
    srcs = ["euler_35.hs"],
    deps = ["//:base", ":helpers"],
)

euler_test(
    name = "euler_35_test",
    problem = 35,
    solution = ":euler_bin35",
)

haskell_binary(
    name = "euler_bin33",
    srcs = ["euler_33.hs"],
    deps = ["//:base"],
)

euler_test(
    name = "euler_33_test",
    problem = 33,
    solution = ":euler_bin33",
)

haskell_binary(
    name = "euler_bin25",
    srcs = ["euler_25.hs"],
    deps = ["//:base"],
)

euler_test(
    name = "euler_25_test",
    problem = 25,
    solution = ":euler_bin25",
)
