library(testthat)
library(ExpDesignR)

test_that("simple randomization is reproducible and supports ratios", {
  x1 <- simple_randomization(100, c("A", "B"), seed = 10)
  x2 <- simple_randomization(100, c("A", "B"), seed = 10)
  expect_identical(x1, x2)
  expect_equal(nrow(x1), 100)
  expect_true(all(x1$Group %in% c("A", "B")))
  y <- simple_randomization(1000, c("A", "B"), ratio = c(3, 1), seed = 1)
  expect_gt(sum(y$Group == "A"), sum(y$Group == "B"))
})

test_that("fixed blocks are balanced", {
  x <- block_randomization(40, c("A", "B"), block_size = 4, seed = 1)
  expect_true(all(vapply(split(x$Group, x$Block), function(z) all(table(z) == 2), logical(1))))
  expect_error(block_randomization(10, c("A", "B"), 4))
})

test_that("variable blocks use permitted sizes", {
  x <- variable_block_randomization(100, c("A", "B"), c(4, 6, 8), seed = 1)
  expect_equal(nrow(x), 100)
  expect_true(all(unique(x$BlockSize) %in% c(4, 6, 8)))
  expect_true(all(table(x$Block) == x$BlockSize[match(names(table(x$Block)), x$Block)]))
})

test_that("stratified randomization preserves rows and creates strata", {
  dat <- data.frame(ID = 1:40, Sex = rep(c("M", "F"), 20), Site = rep(c("A", "B"), each = 20))
  x <- stratified_randomization(dat, c("Sex", "Site"), c("A", "B"), seed = 1)
  expect_equal(nrow(x), nrow(dat)); expect_true(all(c("Treatment", "Stratum") %in% names(x)))
  expect_equal(x$ID, dat$ID)
})

test_that("stratified blocks assign within strata", {
  dat <- data.frame(ID = 1:48, Sex = rep(c("M", "F"), each = 24))
  x <- stratified_block_randomization(dat, "Sex", c("A", "B"), block_size = 4, seed = 2)
  expect_equal(nrow(x), 48)
  expect_true(all(x$Treatment %in% c("A", "B")))
  expect_true(all(table(x$Stratum, x$Treatment) > 0))
})

test_that("cluster randomization preserves cluster IDs", {
  x <- cluster_randomization(letters[1:20], c("A", "B"), seed = 3)
  expect_equal(x$Cluster, letters[1:20]); expect_equal(nrow(x), 20)
  expect_error(cluster_randomization(c("a", "a"), c("A", "B")))
})

test_that("matched-pair randomization assigns one treatment per member", {
  dat <- data.frame(ID = 1:20, Pair = rep(1:10, each = 2))
  x <- matched_pair_randomization(dat, "Pair", c("A", "B"), seed = 4)
  expect_equal(nrow(x), 20)
  expect_true(all(vapply(split(x$Treatment, x$Pair), function(z) length(unique(z)) == 2, logical(1))))
  expect_error(matched_pair_randomization(dat[-20, ], "Pair", c("A", "B")))
})

test_that("restricted randomization controls imbalance", {
  x <- restricted_randomization(100, c("A", "B"), max_imbalance = 1, seed = 5)
  expect_equal(nrow(x), 100)
  expect_lte(abs(diff(table(x$Group))), 1)
})

test_that("minimization and covariate adaptive methods return valid allocations", {
  dat <- data.frame(ID = 1:60, Sex = rep(c("M", "F"), 30), Site = rep(c("A", "B", "C"), 20))
  m <- minimization_randomization(dat, c("Sex", "Site"), seed = 6)
  a <- covariate_adaptive_randomization(dat, c("Sex", "Site"), seed = 6)
  expect_equal(nrow(m), 60); expect_equal(nrow(a), 60)
  expect_true(all(m$Treatment %in% c("Control", "Treatment")))
  expect_true(all(a$Treatment %in% c("Control", "Treatment")))
  expect_error(minimization_randomization(dat, c("Missing")))
})

test_that("additional experimental designs are valid", {
  crd <- completely_randomized_design(30, c("A", "B"), seed = 7)
  rbd <- randomized_block_design(24, c("A", "B"), block_size = 4, seed = 7)
  fac <- factorial_design(list(Dose = c("Low", "High"), Diet = c("A", "B")), replicates = 2, seed = 7)
  sp <- split_plot_design(c("W1", "W2"), c("S1", "S2", "S3"), n_whole = 6, seed = 7)
  expect_equal(nrow(crd), 30); expect_true(all(c("Unit", "Treatment") %in% names(crd)))
  expect_equal(nrow(rbd), 24); expect_true(all(c("Unit", "Block", "Treatment") %in% names(rbd)))
  expect_equal(nrow(fac), 8); expect_true(all(c("Unit", "Dose", "Diet") %in% names(fac)))
  expect_equal(nrow(sp), 18); expect_true(all(c("WholePlot", "WholeTreatment", "SubTreatment") %in% names(sp)))
})

test_that("seed handling does not disturb the caller random state", {
  set.seed(99); before <- runif(1)
  set.seed(99); simple_randomization(10, c("A", "B"), seed = 123); after <- runif(1)
  set.seed(99); expected <- runif(1)
  expect_equal(before, expected); expect_equal(after, expected)
})
