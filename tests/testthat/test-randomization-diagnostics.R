library(testthat)
library(ExpDesignR)

test_that("randomization_diagnostics returns correct structure", {
  x <- simple_randomization(
    n = 100,
    groups = c("A", "B"),
    seed = 123
  )

  d <- randomization_diagnostics(x)

  expect_type(d, "list")
  expect_named(d, c(
    "n",
    "groups",
    "counts",
    "percentages",
    "max_count_difference",
    "max_percentage_difference"
  ))

  expect_equal(d$n, 100)
  expect_equal(d$groups, 2)
  expect_equal(sum(d$counts), 100)
  expect_equal(sum(d$percentages), 100)
  expect_gte(d$max_count_difference, 0)
  expect_gte(d$max_percentage_difference, 0)
})

test_that("randomization_diagnostics gives reproducible results", {
  x1 <- simple_randomization(
    n = 100,
    groups = c("A", "B"),
    seed = 123
  )

  x2 <- simple_randomization(
    n = 100,
    groups = c("A", "B"),
    seed = 123
  )

  expect_identical(
    randomization_diagnostics(x1),
    randomization_diagnostics(x2)
  )
})

test_that("randomization_diagnostics handles three treatment groups", {
  x <- simple_randomization(
    n = 150,
    groups = c("A", "B", "C"),
    seed = 123
  )

  d <- randomization_diagnostics(x)

  expect_equal(d$n, 150)
  expect_equal(d$groups, 3)
  expect_length(d$counts, 3)
  expect_length(d$percentages, 3)
  expect_equal(sum(d$counts), 150)
  expect_equal(sum(d$percentages), 100)
})

test_that("randomization_diagnostics handles unequal allocation ratios", {
  x <- simple_randomization(
    n = 1000,
    groups = c("A", "B"),
    ratio = c(3, 1),
    seed = 123
  )

  d <- randomization_diagnostics(x)

  expect_equal(d$n, 1000)
  expect_equal(d$groups, 2)
  expect_gt(d$counts[["A"]], d$counts[["B"]])
  expect_equal(sum(d$counts), 1000)
})

test_that("randomization_diagnostics supports custom group column", {
  x <- data.frame(
    ID = 1:20,
    Treatment = rep(c("Control", "Treatment"), each = 10)
  )

  d <- randomization_diagnostics(
    x,
    group_col = "Treatment"
  )

  expect_equal(d$n, 20)
  expect_equal(d$groups, 2)
  expect_equal(d$counts[["Control"]], 10)
  expect_equal(d$counts[["Treatment"]], 10)
  expect_equal(d$max_count_difference, 0)
  expect_equal(d$max_percentage_difference, 0)
})

test_that("randomization_diagnostics detects perfectly balanced allocation", {
  x <- data.frame(
    Group = rep(c("A", "B"), each = 25)
  )

  d <- randomization_diagnostics(x)

  expect_equal(d$n, 50)
  expect_equal(d$groups, 2)
  expect_equal(d$counts, c(A = 25L, B = 25L))
  expect_equal(d$percentages, c(A = 50, B = 50))
  expect_equal(d$max_count_difference, 0)
  expect_equal(d$max_percentage_difference, 0)
})

test_that("randomization_diagnostics detects allocation imbalance", {
  x <- data.frame(
    Group = c(rep("A", 70), rep("B", 30))
  )

  d <- randomization_diagnostics(x)

  expect_equal(d$n, 100)
  expect_equal(d$groups, 2)
  expect_equal(d$counts, c(A = 70L, B = 30L))
  expect_equal(d$max_count_difference, 40)
  expect_equal(d$max_percentage_difference, 40)
})

test_that("randomization_diagnostics rejects invalid schedules", {
  expect_error(
    randomization_diagnostics(NULL),
    "schedule.*data frame"
  )

  expect_error(
    randomization_diagnostics(
      data.frame(ID = 1:5)
    ),
    "Column 'Group' not found"
  )

  expect_error(
    randomization_diagnostics(
      data.frame(Group = c("A", "B", NA))
    ),
    "missing"
  )
})
