# This tests the behavior of the various HDF5-based seeds.
# library(testthat); library(DelayedArraySaver); source("test-HDF5ArraySeed.R")

library(HDF5Array)

test_that("HDF5ArraySeeds are saved correctly", {
    X <- writeHDF5Array(matrix(runif(100), ncol=20))
    temp <- tempfile(fileext=".h5")
    saveDelayed(X, temp)

    expect_identical(as.character(rhdf5::h5read(temp, "delayed/path")), as.character(path(X)))

    out <- loadDelayed(temp)
    expect_identical(X, out)
})

test_that("H5SparseMatrixSeeds are saved correctly", {
    h5ad_file <- system.file("extdata", "example_anndata.h5ad", package="zellkonverter")
    test <- H5SparseMatrix(h5ad_file, "obsp/distances")

    temp <- tempfile(fileext=".h5")
    saveDelayed(test, temp)

    expect_identical(as.character(rhdf5::h5read(temp, "delayed/path")), as.character(path(test)))

    out <- loadDelayed(temp)
    expect_identical(test, out)
})

test_that("H5SparseMatrixSeeds are saved correctly", {
    h5ad_file <- system.file("extdata", "krumsiek11.h5ad", package="zellkonverter")
    test <- H5ADMatrix(h5ad_file)

    temp <- tempfile(fileext=".h5")
    saveDelayed(test, temp)

    expect_identical(as.character(rhdf5::h5read(temp, "delayed/path")), as.character(path(test)))

    out <- loadDelayed(temp)
    expect_identical(test, out)
})

test_that("TENxMatrixSeeds are saved correctly", {
    library(DropletTestFiles)
    fname <- getTestFile("tenx-2.0.1-nuclei_900/1.0.0/filtered.h5")
    test <- TENxMatrix(fname, "mm10")

    temp <- tempfile(fileext=".h5")
    saveDelayed(test, temp)

    expect_identical(as.character(rhdf5::h5read(temp, "delayed/path")), as.character(path(test)))

    out <- loadDelayed(temp)
    expect_identical(test, out)
})
