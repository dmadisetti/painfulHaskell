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
    deps = ["@stackage//:hashmap", "//:base"],
    compiler_flags = ["-fprof-auto"],
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

haskell_binary(
    name = "euler_bin23",
    srcs = ["euler_23.hs"],
    deps = ["//:base", ":helpers", "@stackage//:containers"],
)

euler_test(
    name = "euler_23_test",
    problem = 23,
    solution = ":euler_bin23",
)

haskell_binary(
    name = "euler_bin37",
    srcs = ["euler_37.hs"],
    deps = ["//:base", ":helpers", "@stackage//:containers"],
)

euler_test(
    name = "euler_37_test",
    problem = 37,
    solution = ":euler_bin37",
)

haskell_binary(
    name = "euler_bin38",
    srcs = ["euler_38.hs"],
    deps = ["//:base"],
)

euler_test(
    name = "euler_38_test",
    problem = 38,
    solution = ":euler_bin38",
)

haskell_binary(
    name = "euler_bin48",
    srcs = ["euler_48.hs"],
    deps = ["@stackage//:hashmap", "//:base", ":helpers"],
)

euler_test(
    name = "euler_48_test",
    problem = 48,
    solution = ":euler_bin48",
)

haskell_binary(
    name = "euler_bin41",
    srcs = ["euler_41.hs"],
    deps = ["//:base", ":helpers"],
)

euler_test(
    name = "euler_41_test",
    problem = 41,
    solution = ":euler_bin41",
)

haskell_binary(
    name = "euler_bin46",
    srcs = ["euler_46.hs"],
    deps = ["//:base", ":helpers"],
)

euler_test(
    name = "euler_46_test",
    problem = 46,
    solution = ":euler_bin46",
)

haskell_binary(
    name = "euler_bin49",
    srcs = ["euler_49.hs"],
    deps = ["@stackage//:hashmap", "//:base", "helpers"],
)

euler_test(
    name = "euler_49_test",
    problem = 49,
    solution = ":euler_bin49",
)

haskell_binary(
    name = "euler_bin50",
    srcs = ["euler_50.hs"],
    deps = ["@stackage//:hashmap", "//:base", "helpers"],
)

euler_test(
    name = "euler_50_test",
    problem = 50,
    solution = ":euler_bin50",
)

haskell_binary(
    name = "euler_bin29",
    srcs = ["euler_29.hs"],
    deps = ["//:base", "@stackage//:containers"],
)

euler_test(
    name = "euler_29_test",
    problem = 29,
    solution = ":euler_bin29",
)

haskell_binary(
    name = "euler_bin30",
    srcs = ["euler_30.hs"],
    deps = ["@stackage//:hashmap", "//:base"],
)

euler_test(
    name = "euler_30_test",
    problem = 30,
    solution = ":euler_bin30",
)

haskell_binary(
    name = "euler_bin22",
    srcs = ["euler_22.hs"],
    deps = ["//:base"],
    data = ["@euler//:external_euler"],
)

euler_test(
    name = "euler_22_test",
    problem = 22,
    solution = ":euler_bin22",
)

haskell_binary(
    name = "euler_bin34",
    srcs = ["euler_34.hs"],
    deps = ["@stackage//:hashmap", "//:base", "helpers"],
)

euler_test(
    name = "euler_34_test",
    problem = 34,
    solution = ":euler_bin34",
)

haskell_binary(
    name = "euler_bin36",
    srcs = ["euler_36.hs"],
    deps = ["//:base", "helpers"],
)

euler_test(
    name = "euler_36_test",
    problem = 36,
    solution = ":euler_bin36",
)

haskell_binary(
    name = "euler_bin47",
    srcs = ["euler_47.hs"],
    deps = ["@stackage//:hashmap", "//:base", "helpers"],
)

euler_test(
    name = "euler_47_test",
    problem = 47,
    solution = ":euler_bin47",
)

haskell_binary(
    name = "euler_bin44",
    srcs = ["euler_44.hs"],
    deps = ["//:base"],
)

euler_test(
    name = "euler_44_test",
    problem = 44,
    solution = ":euler_bin44",
)

haskell_binary(
    name = "euler_bin39",
    srcs = ["euler_39.hs"],
    deps = ["@stackage//:hashmap", "//:base", "helpers"],
)

euler_test(
    name = "euler_39_test",
    problem = 39,
    solution = ":euler_bin39",
)

haskell_binary(
    name = "euler_bin31",
    srcs = ["euler_31.hs"],
    deps = ["//:base"],
)

euler_test(
    name = "euler_31_test",
    problem = 31,
    solution = ":euler_bin31",
)

haskell_binary(
    name = "euler_bin42",
    srcs = ["euler_42.hs"],
    deps = ["@stackage//:hashmap", "//:base"],
    data = ["@euler//:external_euler"],
)

euler_test(
    name = "euler_42_test",
    problem = 42,
    solution = ":euler_bin42",
)

haskell_binary(
    name = "euler_bin67",
    srcs = ["euler_67.hs"],
    deps = ["//:base"],
    data = ["@euler//:external_euler"],
)

euler_test(
    name = "euler_67_test",
    problem = 67,
    solution = ":euler_bin67",
)

haskell_binary(
    name = "euler_bin26",
    srcs = ["euler_26.hs"],
    deps = ["@stackage//:hashmap", "//:base"],
)

euler_test(
    name = "euler_26_test",
    problem = 26,
    solution = ":euler_bin26",
)

haskell_binary(
    name = "euler_bin19",
    srcs = ["euler_19.hs"],
    deps = ["//:base"],
)

euler_test(
    name = "euler_19_test",
    problem = 19,
    solution = ":euler_bin19",
)

haskell_binary(
    name = "euler_bin45",
    srcs = ["euler_45.hs"],
    deps = ["//:base"],
)

euler_test(
    name = "euler_45_test",
    problem = 45,
    solution = ":euler_bin45",
)

haskell_binary(
    name = "euler_bin43",
    srcs = ["euler_43.hs"],
    deps = ["//:base", "@stackage//:hashmap", "@stackage//:containers"],
)

euler_test(
    name = "euler_43_test",
    problem = 43,
    solution = ":euler_bin43",
)

haskell_binary(
    name = "euler_bin40",
    srcs = ["euler_40.hs"],
    deps = ["//:base"],
)

euler_test(
    name = "euler_40_test",
    problem = 40,
    solution = ":euler_bin40",
)

haskell_binary(
    name = "euler_bin24",
    srcs = ["euler_24.hs"],
    deps = ["//:base"],
)

euler_test(
    name = "euler_24_test",
    problem = 24,
    solution = ":euler_bin24",
)

haskell_binary(
    name = "euler_bin32",
    srcs = ["euler_32.hs"],
    deps = ["//:base", "@stackage//:containers"],
)

euler_test(
    name = "euler_32_test",
    problem = 32,
    solution = ":euler_bin32",
)

haskell_binary(
    name = "euler_bin27",
    srcs = ["euler_27.hs"],
    deps = ["//:base", ":helpers"],
)

euler_test(
    name = "euler_27_test",
    problem = 27,
    solution = ":euler_bin27",
)

haskell_binary(
    name = "euler_bin51",
    srcs = ["euler_51.hs"],
    deps = ["@stackage//:hashable", "@stackage//:hashmap", "//:base", "helpers"],
)

euler_test(
    name = "euler_51_test",
    problem = 51,
    solution = ":euler_bin51",
)

haskell_binary(
    name = "euler_bin52",
    srcs = ["euler_52.hs"],
    deps = ["@stackage//:hashmap", "//:base"],
)

euler_test(
    name = "euler_52_test",
    problem = 52,
    solution = ":euler_bin52",
)

haskell_binary(
    name = "euler_bin59",
    srcs = ["euler_59.hs"],
    data = ["@euler//:external_euler"],
    deps = ["@stackage//:split", "//:base"],
)

euler_test(
    name = "euler_59_test",
    problem = 59,
    solution = ":euler_bin59",
)

haskell_binary(
    name = "euler_bin53",
    srcs = ["euler_53.hs"],
    deps = ["//:base", ":helpers"],
)

euler_test(
    name = "euler_53_test",
    problem = 53,
    solution = ":euler_bin53",
)

haskell_binary(
    name = "euler_bin84",
    srcs = ["euler_84.hs"],
    deps = ["//:base", "@stackage//:hmatrix"],
)

euler_test(
    name = "euler_84_test",
    problem = 84,
    solution = ":euler_bin84",
)

haskell_binary(
    name = "euler_bin63",
    srcs = ["euler_63.hs"],
    deps = ["//:base"],
)

euler_test(
    name = "euler_63_test",
    problem = 63,
    solution = ":euler_bin63",
)

haskell_binary(
    name = "euler_bin79",
    srcs = ["euler_79.hs"],
    deps = ["@stackage//:hashmap", "//:base"],
    data = ["@euler//:external_euler"],
)

euler_test(
    name = "euler_79_test",
    problem = 79,
    solution = ":euler_bin79",
)

haskell_binary(
    name = "euler_bin56",
    srcs = ["euler_56.hs"],
    deps = ["//:base"],
)

euler_test(
    name = "euler_56_test",
    problem = 56,
    solution = ":euler_bin56",
)

haskell_binary(
    name = "euler_bin97",
    srcs = ["euler_97.hs"],
    deps = ["//:base"],
)

euler_test(
    name = "euler_97_test",
    problem = 97,
    solution = ":euler_bin97",
)

haskell_binary(
    name = "euler_bin92",
    srcs = ["euler_92.hs"],
    deps = ["@stackage//:containers", "//:base", ":helpers"],
)

euler_test(
    name = "euler_92_test",
    problem = 92,
    solution = ":euler_bin92",
)

haskell_binary(
    name = "euler_bin55",
    srcs = ["euler_55.hs"],
    deps = ["@stackage//:hashmap", "//:base", ":helpers"],
)

euler_test(
    name = "euler_55_test",
    problem = 55,
    solution = ":euler_bin55",
)

haskell_binary(
    name = "euler_bin57",
    srcs = ["euler_57.hs"],
    deps = ["//:base"],
)

euler_test(
    name = "euler_57_test",
    problem = 57,
    solution = ":euler_bin57",
)
