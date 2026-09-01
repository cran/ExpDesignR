library(testthat)
library(ExpDesignR)


test_that("fixed block randomization produces balanced blocks", {
  
  x <- block_randomization(
    n = 40,
    groups = c("A", "B"),
    block_size = 4,
    seed = 123
  )
  
  expect_equal(nrow(x), 40)
  expect_equal(ncol(x), 3)
  expect_true(all(x$Group %in% c("A", "B")))
  
  # Every block contains 2 subjects in each group
  block_tables <- split(x$Group, x$Block)
  
  expect_true(
    all(
      vapply(
        block_tables,
        function(z) all(table(z) == 2),
        logical(1)
      )
    )
  )
})


test_that("fixed block randomization is reproducible", {
  
  x1 <- block_randomization(
    n = 40,
    groups = c("A", "B"),
    block_size = 4,
    seed = 123
  )
  
  x2 <- block_randomization(
    n = 40,
    groups = c("A", "B"),
    block_size = 4,
    seed = 123
  )
  
  expect_identical(x1, x2)
})


test_that("fixed block randomization supports multiple treatment groups", {
  
  x <- block_randomization(
    n = 30,
    groups = c("A", "B", "C"),
    block_size = 6,
    seed = 123
  )
  
  expect_equal(nrow(x), 30)
  expect_true(all(x$Group %in% c("A", "B", "C")))
  
  block_tables <- split(x$Group, x$Block)
  
  expect_true(
    all(
      vapply(
        block_tables,
        function(z) all(table(z) == 2),
        logical(1)
      )
    )
  )
})


test_that("fixed block randomization rejects non-divisible n", {
  
  expect_error(
    block_randomization(
      n = 10,
      groups = c("A", "B"),
      block_size = 4
    ),
    "'n' must be divisible"
  )
})


test_that("fixed block randomization rejects incompatible block size", {
  
  expect_error(
    block_randomization(
      n = 20,
      groups = c("A", "B", "C"),
      block_size = 4
    ),
    "divisible by the number of groups"
  )
})


test_that("fixed block randomization validates block size", {
  
  expect_error(
    block_randomization(
      n = 20,
      groups = c("A", "B"),
      block_size = 0
    ),
    "positive integer"
  )
  
  expect_error(
    block_randomization(
      n = 20,
      groups = c("A", "B"),
      block_size = 2.5
    ),
    "positive integer"
  )
  
  expect_error(
    block_randomization(
      n = 20,
      groups = c("A", "B"),
      block_size = c(4, 8)
    ),
    "positive integer"
  )
  
  expect_error(
    block_randomization(
      n = 20,
      groups = c("A", "B"),
      block_size = NA
    ),
    "positive integer"
  )
})


test_that("fixed block randomization supports allocation ratios", {
  
  x <- block_randomization(
    n = 40,
    groups = c("A", "B"),
    block_size = 4,
    ratio = c(3, 1),
    seed = 123
  )
  
  expect_equal(nrow(x), 40)
  expect_true(all(x$Group %in% c("A", "B")))
  
  block_tables <- split(x$Group, x$Block)
  
  expect_true(
    all(
      vapply(
        block_tables,
        function(z) {
          tab <- table(z)
          unname(tab["A"]) == 3 &&
            unname(tab["B"]) == 1
        },
        logical(1)
      )
    )
  )
})


test_that("fixed block randomization rejects unrepresentable ratio", {
  
  expect_error(
    block_randomization(
      n = 20,
      groups = c("A", "B"),
      block_size = 4,
      ratio = c(2, 1)
    ),
    "Allocation ratio cannot be represented"
  )
})


test_that("fixed block randomization preserves subject and block structure", {
  
  x <- block_randomization(
    n = 24,
    groups = c("Control", "Treatment"),
    block_size = 4,
    seed = 1
  )
  
  expect_equal(x$Subject, 1:24)
  expect_equal(
    unique(x$Block),
    1:6
  )
  
  expect_true(
    all(table(x$Block) == 4)
  )
  
  expect_equal(
    length(unique(x$Subject)),
    24
  )
})