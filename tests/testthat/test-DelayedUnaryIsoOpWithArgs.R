# This tests the DelayedUnaryIsoOpWithArgs saving/loading functionality.
# library(testthat); library(chihaya); source("test-DelayedUnaryIsoOpWithArgs.R")

library(DelayedArray)
X <- DelayedArray(matrix(runif(100), ncol=20))

test_that("DelayedUnaryIsoOpWithArgs works as expected", {
    Z <- X - runif(5)
    temp <- tempfile(fileext=".h5")
    saveDelayed(Z, temp)

    expect_identical(rhdf5::h5readAttributes(temp, "delayed")$delayed_operation, "unary arithmetic")

    expect_identical(as.vector(rhdf5::h5read(temp, "delayed/method")), "-")
    expect_identical(length(rhdf5::h5read(temp, "delayed/value")), nrow(X))
    expect_identical(as.vector(rhdf5::h5read(temp, "delayed/along")), 0L)
    expect_identical(as.vector(rhdf5::h5read(temp, "delayed/side")), "right")

    roundtrip <- loadDelayed(temp)
    expect_identical(as.matrix(Z), as.matrix(roundtrip))
    expect_s4_class(roundtrip@seed, "DelayedUnaryIsoOpWithArgs")
})

test_that("DelayedUnaryIsoOpWithArgs works along the other dimension", {
    Z <- X - runif(5)
    temp <- tempfile(fileext=".h5")
    saveDelayed(Z, temp)

    # Manually injecting along=1, because we can't actually seem to stage a
    # DelayedArray directly with along=1; calling DelayedArray::sweep does
    # a double-transpose instead.
    rhdf5::h5delete(temp, "delayed/along")
    rhdf5::h5write(1L, temp, "delayed/along")

    vec <- runif(20)
    rhdf5::h5delete(temp, "delayed/value")
    rhdf5::h5write(vec, temp, "delayed/value")

    roundtrip <- loadDelayed(temp)
    expected <- sweep(as.matrix(X), MARGIN=2, STATS=vec, FUN="-")
    expect_identical(as.matrix(expected), as.matrix(roundtrip))
})

test_that("DelayedUnaryIsoOpWithArgs handles logical renaming", {
    Z <- X & runif(5) > 5
    temp <- tempfile(fileext=".h5")
    saveDelayed(Z, temp)

    expect_identical(rhdf5::h5readAttributes(temp, "delayed")$delayed_operation, "unary logic")
    expect_identical(as.vector(rhdf5::h5read(temp, "delayed/method")), "&&")

    roundtrip <- loadDelayed(temp)
    expect_identical(as.matrix(Z), as.matrix(roundtrip))
    expect_s4_class(roundtrip@seed, "DelayedUnaryIsoOpWithArgs")
})

test_that("DelayedUnaryIsoOpWithArgs handles NAs correctly", {
    vec <- runif(5)
    vec[1] <- NA
    Z <- X - vec
    temp <- tempfile(fileext=".h5")
    saveDelayed(Z, temp)
    roundtrip <- loadDelayed(temp)
    expect_identical(as.matrix(Z), as.matrix(roundtrip))

    # Works with non-default NAs
    vec <- 1:5
    Z <- X / vec
    temp <- tempfile(fileext=".h5")
    saveDelayed(Z, temp)

    library(rhdf5)
    (function() {
        fhandle <- H5Fopen(temp)
        on.exit(H5Fclose(fhandle), add=TRUE)
        dhandle <- H5Dopen(fhandle, "delayed/value")
        on.exit(H5Dclose(dhandle), add=TRUE)
        h5writeAttribute(3, dhandle, "missing_placeholder")
    })()

    roundtrip <- loadDelayed(temp)
    vec[3] <- NA
    expected <- X / vec
    expect_identical(as.matrix(expected), as.matrix(roundtrip))
})

test_that("DelayedUnaryIsoOpWithArgs handles side-ness correctly", {
    Z <- runif(5) / X
    temp <- tempfile(fileext=".h5")
    saveDelayed(Z, temp)

    expect_identical(as.vector(rhdf5::h5read(temp, "delayed/method")), "/")
    expect_identical(length(as.vector(rhdf5::h5read(temp, "delayed/value"))), nrow(X))
    expect_identical(as.vector(rhdf5::h5read(temp, "delayed/along")), 0L)
    expect_identical(as.vector(rhdf5::h5read(temp, "delayed/side")), "left")

    roundtrip <- loadDelayed(temp)
    expect_identical(as.matrix(Z), as.matrix(roundtrip))
    expect_s4_class(roundtrip@seed, "DelayedUnaryIsoOpWithArgs")
})

test_that("DelayedUnaryIsoOpWithArgs handles repeated operations correctly (same side)", {
    # Same side
    Z <- (X + runif(5)) + runif(5)
    temp <- tempfile(fileext=".h5")
    saveDelayed(Z, temp)

    expect_identical(as.vector(rhdf5::h5read(temp, "delayed/method")), "+")
    expect_identical(length(as.vector(rhdf5::h5read(temp, "delayed/value"))), nrow(X))
    expect_identical(as.vector(rhdf5::h5read(temp, "delayed/along")), 0L)
    expect_identical(as.vector(rhdf5::h5read(temp, "delayed/side")), "right")

    expect_identical(as.vector(rhdf5::h5read(temp, "delayed/seed/method")), "+")
    expect_identical(length(as.vector(rhdf5::h5read(temp, "delayed/seed/value"))), nrow(X))
    expect_identical(as.vector(rhdf5::h5read(temp, "delayed/seed/along")), 0L)
    expect_identical(as.vector(rhdf5::h5read(temp, "delayed/seed/side")), "right")

    roundtrip <- loadDelayed(temp)
    expect_identical(as.matrix(Z), as.matrix(roundtrip))
    expect_s4_class(roundtrip@seed, "DelayedUnaryIsoOpWithArgs")
})

test_that("DelayedUnaryIsoOpWithArgs handles repeated operations correctly (opposite sides)", {
    # Same side
    Z <- runif(5) + (X + runif(5))
    temp <- tempfile(fileext=".h5")
    saveDelayed(Z, temp)

    expect_identical(as.vector(rhdf5::h5read(temp, "delayed/method")), "+")
    expect_identical(length(as.vector(rhdf5::h5read(temp, "delayed/value"))), nrow(X))
    expect_identical(as.vector(rhdf5::h5read(temp, "delayed/along")), 0L)
    expect_identical(as.vector(rhdf5::h5read(temp, "delayed/side")), "left")

    expect_identical(as.vector(rhdf5::h5read(temp, "delayed/seed/method")), "+")
    expect_identical(length(as.vector(rhdf5::h5read(temp, "delayed/seed/value"))), nrow(X))
    expect_identical(as.vector(rhdf5::h5read(temp, "delayed/seed/along")), 0L)
    expect_identical(as.vector(rhdf5::h5read(temp, "delayed/seed/side")), "right")

    roundtrip <- loadDelayed(temp)
    expect_identical(as.matrix(Z), as.matrix(roundtrip))
    expect_s4_class(roundtrip@seed, "DelayedUnaryIsoOpWithArgs")
})
