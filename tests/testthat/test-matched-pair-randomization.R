library(testthat)
library(ExpDesignR)


test_that("matched-pair randomization assigns one subject to each treatment", {
  
  dat <- data.frame(
    ID = 1:20,
    Pair = rep(1:10, each = 2)
  )
  
  x <- matched_pair_randomization(
    dat,
    pair = "Pair",
    groups = c("Control", "Treatment"),
    seed = 123
  )
  
  expect_equal(nrow(x), 20)
  expect_true("Treatment" %in% names(x))
  expect_true(
    all(x$Treatment %in% c("Control", "Treatment"))
  )
  
  # Exactly one subject from each pair receives each treatment
  pair_tab <- table(x$Pair, x$Treatment)
  
  expect_true(
    all(pair_tab == 1)
  )
})


test_that("matched-pair randomization is reproducible", {
  
  dat <- data.frame(
    ID = 1:20,
    Pair = rep(1:10, each = 2)
  )
  
  x1 <- matched_pair_randomization(
    dat,
    pair = "Pair",
    groups = c("A", "B"),
    seed = 123
  )
  
  x2 <- matched_pair_randomization(
    dat,
    pair = "Pair",
    groups = c("A", "B"),
    seed = 123
  )
  
  expect_identical(x1, x2)
})


test_that("matched-pair randomization changes with different seeds", {
  
  dat <- data.frame(
    ID = 1:20,
    Pair = rep(1:10, each = 2)
  )
  
  x1 <- matched_pair_randomization(
    dat,
    pair = "Pair",
    groups = c("A", "B"),
    seed = 1
  )
  
  x2 <- matched_pair_randomization(
    dat,
    pair = "Pair",
    groups = c("A", "B"),
    seed = 2
  )
  
  expect_false(
    identical(x1$Treatment, x2$Treatment)
  )
})


test_that("matched-pair randomization preserves original data", {
  
  dat <- data.frame(
    ID = 1:10,
    Pair = rep(1:5, each = 2),
    Age = c(21, 22, 30, 31, 40, 41, 50, 51, 60, 61)
  )
  
  x <- matched_pair_randomization(
    dat,
    pair = "Pair",
    groups = c("Control", "Treatment"),
    seed = 123
  )
  
  expect_equal(x$ID, dat$ID)
  expect_equal(x$Pair, dat$Pair)
  expect_equal(x$Age, dat$Age)
  expect_equal(nrow(x), nrow(dat))
})


test_that("matched-pair randomization rejects more than two groups", {
  
  dat <- data.frame(
    ID = 1:6,
    Pair = rep(1:3, each = 2)
  )
  
  expect_error(
    matched_pair_randomization(
      dat,
      pair = "Pair",
      groups = c("A", "B", "C")
    ),
    "exactly two treatment groups"
  )
})


test_that("matched-pair randomization rejects fewer than two groups", {
  
  dat <- data.frame(
    ID = 1:6,
    Pair = rep(1:3, each = 2)
  )
  
  expect_error(
    matched_pair_randomization(
      dat,
      pair = "Pair",
      groups = "A"
    ),
    "exactly two treatment groups"
  )
})


test_that("matched-pair randomization rejects invalid pair column", {
  
  dat <- data.frame(
    ID = 1:6,
    Pair = rep(1:3, each = 2)
  )
  
  expect_error(
    matched_pair_randomization(
      dat,
      pair = "NotAColumn",
      groups = c("A", "B")
    ),
    "pair.*column"
  )
})


test_that("matched-pair randomization rejects missing pair IDs", {
  
  dat <- data.frame(
    ID = 1:6,
    Pair = c(1, 1, 2, 2, NA, 3)
  )
  
  expect_error(
    matched_pair_randomization(
      dat,
      pair = "Pair",
      groups = c("A", "B")
    ),
    "Pair IDs.*missing"
  )
})


test_that("matched-pair randomization requires exactly two subjects per pair", {
  
  dat <- data.frame(
    ID = 1:5,
    Pair = c(1, 1, 2, 2, 2)
  )
  
  expect_error(
    matched_pair_randomization(
      dat,
      pair = "Pair",
      groups = c("A", "B")
    ),
    "exactly two subjects"
  )
})