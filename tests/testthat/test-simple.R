# This tests various odds and ends not covered by other tests.
# library(testthat); library(chihaya); source("test-simple.R")

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

test_that("saving of a logical array works correctly", {
    x0 <- matrix(runif(200), ncol=20)

    # Preserves logical arrays.
    x <- DelayedArray(x0 > 0.5)
    tmp <- tempfile(fileext=".h5")
    saveDelayed(x, tmp)

    arra <- rhdf5::h5readAttributes(tmp, "delayed/data")
    expect_identical(1L, arra$is_boolean)
    out <- loadDelayed(tmp)
    expect_identical(x, out)
})

test_that("missing values in character arrays are respected", {
    x0 <- matrix(sample(LETTERS, 100, replace=TRUE), 5, 20)
    x <- DelayedArray(x0)
    tmp <- tempfile(fileext=".h5")
    saveDelayed(x, tmp)

    arra <- rhdf5::h5readAttributes(tmp, "delayed/data")
    expect_null(arra[["missing_placeholder"]])
    out <- loadDelayed(tmp)
    expect_identical(x, out)

    # Throwing in some missing values.
    x0[1] <- "NA"
    x0[100] <- NA
    x <- DelayedArray(x0)
    tmp <- tempfile(fileext=".h5")
    saveDelayed(x, tmp)

    arra <- rhdf5::h5readAttributes(tmp, "delayed/data")
    expect_identical("_NA", arra[["missing_placeholder"]])
    out <- loadDelayed(tmp)
    expect_identical(x, out)

    # Back-compatibility check.
    x0 <- matrix(sample(LETTERS, 100, replace=TRUE), 5, 20)
    x <- DelayedArray(x0)
    tmp <- tempfile(fileext=".h5")
    saveDelayed(x, tmp)

    library(rhdf5)
    (function() {
        fhandle <- H5Fopen(tmp)
        on.exit(H5Fclose(fhandle), add=TRUE)
        dhandle <- H5Dopen(fhandle, "delayed/data")
        on.exit(H5Dclose(dhandle), add=TRUE)
        h5writeAttribute("Z", dhandle, "missing-value-placeholder")
    })()

    copy <- x0
    copy[copy=="Z"] <- NA
    out <- loadDelayed(tmp)
    expect_identical(copy, as.matrix(out))
})

test_that("missing values in other array types are respected", {
    # integer.
    x0 <- matrix(as.integer(rpois(100, 1)), 5, 20)
    x0[1,1] <- NA
    x <- DelayedArray(x0)
    tmp <- tempfile(fileext=".h5")
    saveDelayed(x, tmp)

    arra <- rhdf5::h5readAttributes(tmp, "delayed/data")
    expect_identical(arra[["missing_placeholder"]], NA_integer_)
    out <- loadDelayed(tmp)
    expect_identical(x, out)

    # double.
    x0 <- matrix(rnorm(100, 1), 5, 20)
    x0[1,1] <- NA
    x <- DelayedArray(x0)
    tmp <- tempfile(fileext=".h5")
    saveDelayed(x, tmp)

    arra <- rhdf5::h5readAttributes(tmp, "delayed/data")
    expect_identical(arra[["missing_placeholder"]], NA_real_)
    out <- loadDelayed(tmp)
    expect_identical(x, out)

    # logical
    x0 <- matrix(rnorm(100, 1) > 0, 5, 20)
    x0[1,1] <- NA
    x <- DelayedArray(x0)
    tmp <- tempfile(fileext=".h5")
    saveDelayed(x, tmp)

    arra <- rhdf5::h5readAttributes(tmp, "delayed/data")
    expect_identical(arra[["missing_placeholder"]], -1L)
    out <- loadDelayed(tmp)
    expect_identical(x, out)
})

test_that("non-default missing placeholders are respected", {
    # integer.
    x0 <- matrix(as.integer(rpois(100, 1)), 5, 20)
    x <- DelayedArray(x0)
    tmp <- tempfile(fileext=".h5")
    saveDelayed(x, tmp)

    library(rhdf5)
    (function() {
        fhandle <- H5Fopen(tmp)
        on.exit(H5Fclose(fhandle), add=TRUE)
        dhandle <- H5Dopen(fhandle, "delayed/data")
        on.exit(H5Dclose(dhandle), add=TRUE)
        h5writeAttribute(1L, dhandle, "missing_placeholder")
    })()

    out <- loadDelayed(tmp)
    copy <- x0
    copy[copy == 1L] <- NA
    expect_identical(as.matrix(out), copy)

    # NaN special case.
    x0 <- matrix(rnorm(100, 1), 5, 20)
    x0[1] <- NaN
    x <- DelayedArray(x0)
    tmp <- tempfile(fileext=".h5")
    saveDelayed(x, tmp)

    library(rhdf5)
    (function() {
        fhandle <- H5Fopen(tmp)
        on.exit(H5Fclose(fhandle), add=TRUE)
        dhandle <- H5Dopen(fhandle, "delayed/data")
        on.exit(H5Dclose(dhandle), add=TRUE)
        h5writeAttribute(NaN, dhandle, "missing_placeholder")
    })()

    out <- loadDelayed(tmp)
    copy <- x0
    copy[1] <- NA
    expect_identical(as.matrix(out), copy)
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

    # Throwing in an NA and an NaN.
    copy <- x0
    copy@x[1] <- NA
    copy@x[2] <- NaN
    x <- DelayedArray(copy)
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

    # Throwing in an NA.
    copy <- x0
    copy@x[1] <- NA
    x <- DelayedArray(copy)
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

    arra <- rhdf5::h5readAttributes(tmp, "delayed/data")
    expect_identical(1L, arra$is_boolean)

    out <- loadDelayed(tmp)
    expect_identical(x, out)

    # Throwing in an NA.
    copy <- x0
    copy@x[1] <- NA
    x <- DelayedArray(copy)
    tmp <- tempfile(fileext=".h5")
    saveDelayed(x, tmp)
    out <- loadDelayed(tmp)
    expect_identical(x, out)
})

test_that("type chooser works correctly", {
    expect_identical(chihaya:::get_best_type_double(c(1.2, 2.3)), "H5T_NATIVE_DOUBLE")
    expect_identical(chihaya:::get_best_type_double(c(1, NaN, 2)), "H5T_NATIVE_DOUBLE")
    expect_identical(chihaya:::get_best_type_double(c(1, NA, 2)), "H5T_NATIVE_DOUBLE")
    expect_identical(chihaya:::get_best_type_double(c(1, 2)), "H5T_NATIVE_UINT8")
    expect_identical(chihaya:::get_best_type_double(c(-1, 2)), "H5T_NATIVE_INT8")
    expect_identical(chihaya:::get_best_type_double(c(1000, 2)), "H5T_NATIVE_UINT16")
    expect_identical(chihaya:::get_best_type_double(c(-1, 2000)), "H5T_NATIVE_INT16")
    expect_identical(chihaya:::get_best_type_double(c(100000)), "H5T_NATIVE_INT32")
    expect_identical(chihaya:::get_best_type_double(c(-100000)), "H5T_NATIVE_INT32")
    expect_identical(chihaya:::get_best_type_double(numeric(0)), "H5T_NATIVE_DOUBLE")
    expect_identical(chihaya:::get_best_type_double(c(5e9)), "H5T_NATIVE_DOUBLE")
    expect_identical(chihaya:::get_best_type_double(c(-5e9)), "H5T_NATIVE_DOUBLE")

    expect_identical(chihaya:::get_best_type_int(c(1L, NA, 2L)), "H5T_NATIVE_INT32")
    expect_identical(chihaya:::get_best_type_int(c(1L, 2L)), "H5T_NATIVE_UINT8")
    expect_identical(chihaya:::get_best_type_int(c(-1L, 2L)), "H5T_NATIVE_INT8")
    expect_identical(chihaya:::get_best_type_int(c(1000L, 2L)), "H5T_NATIVE_UINT16")
    expect_identical(chihaya:::get_best_type_int(c(-1L, 2000L)), "H5T_NATIVE_INT16")
    expect_identical(chihaya:::get_best_type_int(c(100000L)), "H5T_NATIVE_INT32")
    expect_identical(chihaya:::get_best_type_int(c(-100000L)), "H5T_NATIVE_INT32")
    expect_identical(chihaya:::get_best_type_int(integer(0)), "H5T_NATIVE_INT32")
})
