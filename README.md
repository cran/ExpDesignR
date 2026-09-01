# ExpDesignR

**Experimental Design and Randomization Methods for Biomedical and Veterinary Research**

ExpDesignR provides reproducible tools for treatment allocation and experimental design. Version 1.0.0 establishes the first stable API for simple, blocked, stratified, cluster, matched-pair, restricted, and covariate-adaptive randomization, together with common experimental designs and allocation utilities.

## Randomization

```r
simple_randomization(100, c("Control", "Treatment"), seed = 123)
block_randomization(100, c("Control", "Treatment"), block_size = 4, seed = 123)
variable_block_randomization(100, c("Control", "Treatment"), c(4, 6, 8), seed = 123)
stratified_randomization(dat, "Sex", c("Control", "Treatment"), seed = 123)
stratified_block_randomization(dat, "Sex", c("Control", "Treatment"), 4, seed = 123)
cluster_randomization(paste0("Site_", 1:20), c("Control", "Treatment"), seed = 123)
matched_pair_randomization(dat, "Pair", c("Control", "Treatment"), seed = 123)
restricted_randomization(100, c("Control", "Treatment"), max_imbalance = 1, seed = 123)
minimization_randomization(dat, c("Sex", "Site"), seed = 123)
covariate_adaptive_randomization(dat, c("Sex", "Site"), seed = 123)
```

## Experimental designs

```r
completely_randomized_design(40, c("A", "B"), seed = 123)
randomized_block_design(40, c("A", "B"), block_size = 4, seed = 123)
factorial_design(list(Dose = c("Low", "High"), Diet = c("A", "B")), replicates = 3, seed = 123)
latin_square(LETTERS[1:4], seed = 123)
crossover_design(c("A", "B"), subjects = 20, periods = 2, seed = 123)
```

## Utilities

```r
allocation_summary(schedule)
plot_randomization(schedule)
export_schedule(schedule, tempfile(fileext = ".csv"))
```

The package uses established principles of randomization and experimental design; see Rosenberger and Lachin (2015) and Jones and Kenward (2014).
