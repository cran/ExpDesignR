#===========================================================
# ExpDesignR
# Unit Tests - Core Randomization Functions
#===========================================================

library(testthat)
library(ExpDesignR)

#===========================================================
# simple_randomization()
#===========================================================

test_that("simple_randomization returns correct dimensions", {
  
  x <- simple_randomization(
    n = 100,
    groups = c("A","B"),
    seed = 123
  )
  
  expect_s3_class(x, "data.frame")
  expect_equal(nrow(x),100)
  expect_equal(ncol(x),2)
  
})

test_that("simple_randomization creates requested groups",{
  
  x <- simple_randomization(
    n = 50,
    groups = c("A","B","C"),
    seed = 1
  )
  
  expect_true(all(unique(x$Group) %in%
                    c("A","B","C")))
  
})

test_that("simple_randomization rejects invalid n",{
  
  expect_error(
    simple_randomization(
      n=-5,
      groups=c("A","B")
    )
  )
  
})
#===========================================================
# Additional tests - simple_randomization()
#===========================================================

test_that("simple_randomization works with three groups", {
  
  x <- simple_randomization(
    n = 90,
    groups = c("A", "B", "C"),
    seed = 123
  )
  
  expect_equal(nrow(x), 90)
  expect_true(all(unique(x$Group) %in% c("A","B","C")))
  
})

test_that("simple_randomization is reproducible", {
  
  x1 <- simple_randomization(
    n = 50,
    groups = c("A","B"),
    seed = 100
  )
  
  x2 <- simple_randomization(
    n = 50,
    groups = c("A","B"),
    seed = 100
  )
  
  expect_identical(x1, x2)
  
})

test_that("simple_randomization accepts numeric groups", {
  
  x <- simple_randomization(
    n = 40,
    groups = c(1,2),
    seed = 1
  )
  
  expect_equal(nrow(x),40)
  
})

test_that("simple_randomization rejects one group", {
  
  expect_error(
    
    simple_randomization(
      n = 20,
      groups = "A"
    )
    
  )
  
})
#===========================================================
# block_randomization()
#===========================================================

test_that("block_randomization returns correct size",{
  
  x <- block_randomization(
    n=40,
    groups=c("A","B"),
    block_size=4,
    seed=123
  )
  
  expect_equal(nrow(x),40)
  
})

test_that("each block is balanced", {
  
  x <- block_randomization(
    n = 40,
    groups = c("A","B"),
    block_size = 4,
    seed = 123
  )
  
  blocks <- split(x, x$Block)
  
  for (b in blocks) {
    
    counts <- table(b$Group)
    
    expect_true(all(counts == 2))
    
  }
  
})

test_that("invalid block size throws error",{
  
  expect_error(
    
    block_randomization(
      n=20,
      groups=c("A","B"),
      block_size=3
    )
    
  )
  
})

test_that("three-group block randomization", {
  
  x <- block_randomization(
    
    n=36,
    
    groups=c("A","B","C"),
    
    block_size=6
    
  )
  
  expect_equal(nrow(x),36)
  
})

#===========================================================
# Additional tests - block_randomization()
#===========================================================

test_that("three-group block randomization", {
  
  x <- block_randomization(
    n = 36,
    groups = c("A","B","C"),
    block_size = 6,
    seed = 1
  )
  
  expect_equal(nrow(x),36)
  
})

test_that("three-group blocks are balanced", {
  
  x <- block_randomization(
    n = 36,
    groups = c("A","B","C"),
    block_size = 6,
    seed = 1
  )
  
  blocks <- split(x, x$Block)
  
  for(b in blocks){
    
    expect_true(all(table(b$Group) == 2))
    
  }
  
})

test_that("block_randomization rejects invalid n", {
  
  expect_error(
    
    block_randomization(
      n = 0,
      groups = c("A","B")
    )
    
  )
  
})

test_that("block_randomization rejects one group", {
  
  expect_error(
    
    block_randomization(
      n = 20,
      groups = "A"
    )
    
  )
  
})

test_that("block_randomization rejects n not divisible by block size", {
  
  expect_error(
    
    block_randomization(
      n = 22,
      groups = c("A","B"),
      block_size = 4
    )
    
  )
  
})
#===========================================================
# stratified_randomization()
#===========================================================

test_that("stratified_randomization works",{
  
  dat <- data.frame(
    
    ID=1:20,
    
    Sex=rep(c("M","F"),each=10)
    
  )
  
  x <- stratified_randomization(
    
    data=dat,
    
    strata="Sex",
    
    groups=c("Control","Treatment"),
    
    seed=1
    
  )
  
  expect_equal(nrow(x),20)
  
  expect_true("Treatment" %in% names(x))
  
})
test_that("multiple strata work", {
  
  dat <- data.frame(
    
    ID=1:40,
    
    Sex=rep(c("M","F"),20),
    
    Breed=rep(c("A","B"),20)
    
  )
  
  x <- stratified_randomization(
    
    dat,
    
    strata=c("Sex","Breed"),
    
    groups=c("Control","Treatment")
    
  )
  
  expect_equal(nrow(x),40)
  
})

test_that("missing strata variable", {
  
  dat <- data.frame(ID=1:10)
  
  expect_error(
    
    stratified_randomization(
      
      dat,
      
      strata="Sex",
      
      groups=c("A","B")
      
    )
    
  )
  
})

#===========================================================
# Additional tests - stratified_randomization()
#===========================================================

test_that("multiple strata work", {
  
  dat <- data.frame(
    
    ID = 1:40,
    Sex = rep(c("M","F"),20),
    Breed = rep(c("Large","Small"),20)
    
  )
  
  x <- stratified_randomization(
    
    data = dat,
    strata = c("Sex","Breed"),
    groups = c("Control","Treatment"),
    seed = 1
    
  )
  
  expect_equal(nrow(x),40)
  
})

test_that("stratified_randomization rejects non-data frame", {
  
  expect_error(
    
    stratified_randomization(
      
      data = 100,
      strata = "Sex",
      groups = c("A","B")
      
    )
    
  )
  
})

test_that("missing strata variable", {
  
  dat <- data.frame(ID=1:10)
  
  expect_error(
    
    stratified_randomization(
      
      data = dat,
      strata = "Sex",
      groups = c("A","B")
      
    )
    
  )
  
})

test_that("requires at least two groups", {
  
  dat <- data.frame(
    
    ID = 1:10,
    Sex = rep(c("M","F"),5)
    
  )
  
  expect_error(
    
    stratified_randomization(
      
      dat,
      strata = "Sex",
      groups = "A"
      
    )
    
  )
  
})
#===========================================================
# cluster_randomization()
#===========================================================

test_that("cluster_randomization works",{
  
  x <- cluster_randomization(
    
    clusters=LETTERS[1:20],
    
    groups=c("A","B"),
    
    seed=100
    
  )
  
  expect_equal(nrow(x),20)
  
  expect_true(all(
    
    unique(x$Group) %in%
      
      c("A","B")
    
  ))
  
})

test_that("numeric clusters work", {
  
  x <- cluster_randomization(
    1:20,
    c("A","B")
  )
  
  expect_equal(nrow(x),20)
  
})

test_that("one cluster errors", {
  
  expect_error(
    
    cluster_randomization(
      1,
      c("A","B")
    )
    
  )
  
})

#===========================================================
# Additional tests - cluster_randomization()
#===========================================================

test_that("numeric clusters work", {
  
  x <- cluster_randomization(
    
    clusters = 1:20,
    groups = c("A","B"),
    seed = 123
    
  )
  
  expect_equal(nrow(x),20)
  
})

test_that("three treatment groups work", {
  
  x <- cluster_randomization(
    
    clusters = paste0("Cluster_", 1:30),
    groups = c("A", "B", "C"),
    seed = 123
    
  )
  
  expect_equal(nrow(x), 30)
  expect_true(all(x$Cluster == paste0("Cluster_", 1:30)))
  expect_true(all(x$Group %in% c("A", "B", "C")))
  
})

test_that("cluster_randomization rejects one group", {
  
  expect_error(
    
    cluster_randomization(
      
      clusters = LETTERS[1:10],
      groups = "A"
      
    )
    
  )
  
})

test_that("cluster_randomization rejects too few clusters", {
  
  expect_error(
    
    cluster_randomization(
      
      clusters = 1,
      groups = c("A","B")
      
    )
    
  )
  
})

test_that("cluster_randomization is reproducible", {
  
  x1 <- cluster_randomization(
    
    clusters = LETTERS[1:20],
    groups = c("A","B"),
    seed = 100
    
  )
  
  x2 <- cluster_randomization(
    
    clusters = LETTERS[1:20],
    groups = c("A","B"),
    seed = 100
    
  )
  
  expect_identical(x1,x2)
  
})
#===========================================================
# latin_square()
#===========================================================

test_that("latin_square dimensions correct",{
  
  x <- latin_square(
    
    LETTERS[1:5],
    
    randomize=FALSE
    
  )
  
  expect_equal(nrow(x),5)
  
  expect_equal(ncol(x),5)
  
})

test_that("latin_square contains all treatments",{
  
  trt <- LETTERS[1:5]
  
  x <- latin_square(
    
    trt,
    
    randomize=FALSE
    
  )
  
  expect_equal(
    
    sort(unique(as.vector(x))),
    
    sort(trt)
    
  )
  
})

test_that("latin_square randomization works", {
  
  x <- latin_square(
    LETTERS[1:4],
    randomize = TRUE,
    seed = 123
  )
  
  expect_equal(nrow(x),4)
  expect_equal(ncol(x),4)
  
})

test_that("duplicate treatments throw error", {
  
  expect_error(
    
    latin_square(
      c("A","A","B")
    )
    
  )
  
})

test_that("single treatment throws error", {
  
  expect_error(
    
    latin_square("A")
    
  )
  
})

test_that("all treatments appear once per row", {
  
  x <- latin_square(
    LETTERS[1:5],
    randomize = FALSE
  )
  
  for(i in 1:5)
    expect_equal(length(unique(x[i, ])),5)
  
})

test_that("all treatments appear once per column", {
  
  x <- latin_square(
    LETTERS[1:5],
    randomize = FALSE
  )
  
  for(i in 1:5)
    expect_equal(length(unique(x[, i])),5)
  
})
#===========================================================
# crossover_design()
#===========================================================

test_that("crossover design dimensions",{
  
  x <- crossover_design(
    
    treatments=c("A","B"),
    
    subjects=20,
    
    periods=2,
    
    seed=1
    
  )
  
  expect_equal(nrow(x),20)
  
  expect_equal(ncol(x),3)
  
})

test_that("crossover contains treatment labels",{
  
  x <- crossover_design(
    
    treatments=c("A","B"),
    
    subjects=10,
    
    periods=2,
    
    seed=2
    
  )
  
  vals <- unique(
    
    unlist(
      
      x[,2:3]
      
    )
    
  )
  
  expect_true(
    
    all(vals %in%
          
          c("A","B"))
    
  )
  
})

test_that("duplicate treatments error", {
  
  expect_error(
    
    crossover_design(
      c("A","A"),
      subjects = 10
    )
    
  )
  
})

test_that("subjects less than two", {
  
  expect_error(
    
    crossover_design(
      c("A","B"),
      subjects = 1
    )
    
  )
  
})

test_that("periods less than two", {
  
  expect_error(
    
    crossover_design(
      c("A","B"),
      subjects = 10,
      periods = 1
    )
    
  )
  
})
#===========================================================
# allocation_summary()
#===========================================================

test_that("allocation_summary works",{
  
  sch <- simple_randomization(
    
    n=30,
    
    groups=c("A","B"),
    
    seed=100
    
  )
  
  out <- allocation_summary(sch)
  
  expect_true(
    
    all(
      
      c("Group","Count","Percentage")
      
      %in%
        
        names(out)
      
    )
    
  )
  
})

test_that("missing group column", {
  
  df <- data.frame(A=1:5)
  
  expect_error(
    
    allocation_summary(
      df
    )
    
  )
  
})

test_that("invalid object", {
  
  expect_error(
    
    allocation_summary(123)
    
  )
  
})
#===========================================================
# export_schedule()
#===========================================================

test_that("export_schedule creates file",{
  
  sch <- simple_randomization(
    
    n=20,
    
    groups=c("A","B"),
    
    seed=1
    
  )
  
  tf <- tempfile(fileext=".csv")
  
  export_schedule(
    
    sch,
    
    file=tf
    
  )
  
  expect_true(file.exists(tf))
  
})

test_that("invalid object errors", {
  
  expect_error(
    
    export_schedule(123)
    
  )
  
})

test_that("invalid filename errors", {
  
  sch <- simple_randomization(
    10,
    c("A","B")
  )
  
  expect_error(
    
    export_schedule(
      sch,
      file = 10
    )
    
  )
  
})
#===========================================================
# plot_randomization()
#===========================================================

test_that("plot_randomization returns ggplot",{
  
  sch <- simple_randomization(
    
    n=40,
    
    groups=c("A","B"),
    
    seed=10
    
  )
  
  p <- plot_randomization(sch)
  
  expect_s3_class(
    
    p,
    
    "ggplot"
    
  )
  
})

test_that("missing column errors", {
  
  df <- data.frame(A=1:5)
  
  expect_error(
    
    plot_randomization(df)
    
  )
  
})

test_that("invalid object errors", {
  
  expect_error(
    
    plot_randomization(123)
    
  )
  
})