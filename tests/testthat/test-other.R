# This tests various odds and ends not covered by other tests.
# library(testthat); library(chihaya); source("test-other.R")

library(S4Vectors)
library(Matrix)

test_that("saving of an array works correctly", {
    x0 <- matrix(runif(200), ncol=20)
    x <- DelayedArray(x0)
    tmp <- tempfile(fileext=".h5")
    saveDelayed(x, tmp)

    arra <- rhdf5::h5read(tmp, "delayed/data")
    expect_identical(x0, arra)
    out <- loadDelayed(tmp)
    expect_identical(x, out)

    # Handles dimnames.
    dimnames(x0) <- list(1:nrow(x), head(letters, ncol(x)))
    x <- DelayedArray(x0)

    tmp <- tempfile(fileext=".h5")
    saveDelayed(x, tmp)

    expect_identical(as.vector(rhdf5::h5read(tmp, "delayed/dimnames/0")), rownames(x))
    expect_identical(as.vector(rhdf5::h5read(tmp, "delayed/dimnames/1")), colnames(x))

    out <- loadDelayed(tmp)
    expect_identical(x, out)

    # Handles some missing dimnames.
    rownames(x0) <- NULL
    x <- DelayedArray(x0)

    tmp <- tempfile(fileext=".h5")
    saveDelayed(x, tmp)

    out <- loadDelayed(tmp)
    expect_identical(x, out)
})

test_that("saving of a CsparseMatrix works correctly", {
    x0 <- rsparsematrix(20, 10, 0.1)
    x <- DelayedArray(x0)
    tmp <- tempfile(fileext=".h5")
    saveDelayed(x, tmp)

    # Check that it follows H5SparseMatrix conventions.
    library(HDF5Array)
    stuff <- H5SparseMatrix(tmp, "delayed")
    expect_identical(unname(as.matrix(stuff)), unname(as.matrix(x)))

    out <- loadDelayed(tmp)
    expect_identical(x, out)

    # Supports dimnames.
    dimnames(x0) <- list(LETTERS[1:20], letters[1:10])
    x <- DelayedArray(x0)
    tmp <- tempfile(fileext=".h5")
    saveDelayed(x, tmp)

    out <- loadDelayed(tmp)
    expect_identical(x, out)
})

test_that("saving of a CsparseMatrix works correctly with integers", {
    x0 <- round(rsparsematrix(20, 10, 0.1) * 10)
    x <- DelayedArray(x0)
    tmp <- tempfile(fileext=".h5")
    saveDelayed(x, tmp)

    library(HDF5Array)
    stuff <- H5SparseMatrix(tmp, "delayed")
    expect_identical(type(stuff), "integer")
    expect_equal(unname(as.matrix(stuff)), unname(as.matrix(x)))

    out <- loadDelayed(tmp)
    expect_identical(x, out)

    # Trying with larger integers.
    x <- DelayedArray(x0 * 10000)
    tmp <- tempfile(fileext=".h5")
    saveDelayed(x, tmp)
    out <- loadDelayed(tmp)
    expect_identical(x, out)
})

test_that("saving of a CsparseMatrix works correctly with logicals", {
    x0 <- rsparsematrix(20, 10, 0.1) != 0
    x <- DelayedArray(x0)
    tmp <- tempfile(fileext=".h5")
    saveDelayed(x, tmp)

    library(HDF5Array)
    stuff <- H5SparseMatrix(tmp, "delayed")
    expect_identical(type(stuff), "logical")

    out <- loadDelayed(tmp)
    expect_identical(x, out)
})

test_that("type chooser works correctly", {
    expect_identical(chihaya:::get_best_type(c(1.2, 2.3)), "H5T_NATIVE_DOUBLE")
    expect_identical(chihaya:::get_best_type(c(1, 2)), "H5T_NATIVE_USHORT")
    expect_identical(chihaya:::get_best_type(c(-1, 2)), "H5T_NATIVE_SHORT")
    expect_identical(chihaya:::get_best_type(c(100000)), "H5T_NATIVE_UINT")
    expect_identical(chihaya:::get_best_type(c(-100000)), "H5T_NATIVE_INT")
    expect_identical(chihaya:::get_best_type(numeric(0)), "H5T_NATIVE_USHORT")
    expect_identical(chihaya:::get_best_type(c(5e9)), "H5T_NATIVE_ULONG")
    expect_identical(chihaya:::get_best_type(c(-5e9)), "H5T_NATIVE_LONG")
})

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

