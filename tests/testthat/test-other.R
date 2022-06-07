# This tests various odds and ends not covered by other tests.
# library(testthat); library(chihaya); source("test-other.R")

library(S4Vectors)
library(Matrix)

test_that("saving of a LowRankMatrix works correctly", {
    left <- matrix(rnorm(100000), ncol=20)
    right <- matrix(rnorm(50000), ncol=20)

    library(BiocSingular)
    thing <- LowRankMatrix(left, right)

    # Round-trips properly.
    temp <- tempfile()
    saveDelayed(thing, temp)
    out <- loadDelayed(temp)
    expect_identical(thing, out)
})

test_that("saving of a ResidualMatrix works correctly", {
    y <- rsparsematrix(80, 50, 0.5)
    design <- model.matrix(~gl(8, 10))
    thing <- ResidualMatrix::ResidualMatrix(y, design=design)

    # Round-trips properly.
    temp <- tempfile()
    saveDelayed(thing, temp)
    out <- loadDelayed(temp)
    expect_identical(thing, out)
    expect_s4_class(out, "ResidualMatrix")

    # Works when transposed.
    thing2 <- t(thing)
    temp2 <- tempfile()
    saveDelayed(thing2, temp2)
    out <- loadDelayed(temp2)
    expect_identical(thing2, out)
    expect_s4_class(out, "ResidualMatrix")

    # Same result if we ignore the type hint.
    current <- knownOperations()
    current[["residual matrix"]] <- NULL
    old <- knownOperations(current)
    on.exit(knownOperations(old))

    out <- loadDelayed(temp)
    expect_false(is(out, "ResidualMatrix"))
    expect_identical(unname(as.matrix(thing)), unname(as.matrix(out)))
            
    out <- loadDelayed(temp2)
    expect_false(is(out, "ResidualMatrix"))
    expect_identical(unname(as.matrix(thing2)), unname(as.matrix(out)))
})

