# DelayedArrays to HDF5

## Overview

Save delayed operations to HDF5 using the [**chihaya**](https://github.com/ArtifactDB/chihaya) specification.
This extracts operations out of a [`DelayedArray`](https://bioconductor.org/packages/DelayedArray) and stores them in a HDF5 file,
where they can be used to reconstitute the same `DelayedArray` in a new R session - or indeed, in a different analysis framework altogether.
The idea is to save the operations, which is usually cheap;
rather than the results of the operations, which may be expensive for large datasets or when sparsity is broken.

## Quick start

If we make a `DelayedArray` with arbitrary operations:

```r
library(DelayedArray)
x <- DelayedArray(matrix(runif(1000), ncol=10))
x <- x[11:15,] / runif(5) 
x <- log2(x + 1)
x
## <5 x 10> matrix of class DelayedMatrix and type "double":
##             [,1]        [,2]        [,3] ...       [,9]      [,10]
## [1,] 1.318228112 1.789374232 1.854133153   . 1.10085064 1.22825033
## [2,] 0.340258109 0.598988926 0.005719794   . 0.05900444 0.19562976
## [3,] 0.205758979 0.624928389 0.574661104   . 0.96990885 0.31573385
## [4,] 0.129171362 1.149253865 0.091821910   . 0.10878614 0.45618400
## [5,] 1.317402933 1.753933055 1.857993438   . 1.83012744 2.11469960
```

We can save it to file with the **chihaya** R package:

```r
library(chihaya)
fpath <- tempfile(fileext=".h5")
saveDelayed(x, fpath, "my_delayed_array")
rhdf5::h5ls(fpath)
##                                     group             name       otype  dclass      dim
## 0                                       / my_delayed_array   H5I_GROUP
## 1                       /my_delayed_array             base H5I_DATASET   FLOAT    ( 0 )
## 2                       /my_delayed_array           method H5I_DATASET  STRING    ( 0 )
## 3                       /my_delayed_array             seed   H5I_GROUP
## 4                  /my_delayed_array/seed           method H5I_DATASET  STRING    ( 0 )
## 5                  /my_delayed_array/seed             seed   H5I_GROUP
## 6             /my_delayed_array/seed/seed            along H5I_DATASET INTEGER    ( 0 )
## 7             /my_delayed_array/seed/seed           method H5I_DATASET  STRING    ( 0 )
## 8             /my_delayed_array/seed/seed             seed   H5I_GROUP
## 9        /my_delayed_array/seed/seed/seed            index   H5I_GROUP
## 10 /my_delayed_array/seed/seed/seed/index                0 H5I_DATASET INTEGER        5
## 11       /my_delayed_array/seed/seed/seed             seed   H5I_GROUP
## 12  /my_delayed_array/seed/seed/seed/seed             data H5I_DATASET   FLOAT 100 x 10
## 13  /my_delayed_array/seed/seed/seed/seed           native H5I_DATASET INTEGER    ( 0 )
## 14            /my_delayed_array/seed/seed             side H5I_DATASET  STRING    ( 0 )
## 15            /my_delayed_array/seed/seed            value H5I_DATASET   FLOAT        5
## 16                 /my_delayed_array/seed             side H5I_DATASET  STRING    ( 0 )
## 17                 /my_delayed_array/seed            value H5I_DATASET   FLOAT    ( 0 )
```

And then reload it in a separate session:

```r
y <- loadDelayed(fpath, "my_delayed_array")
y
## <5 x 10> matrix of class DelayedMatrix and type "double":
##             [,1]        [,2]        [,3] ...       [,9]      [,10]
## [1,] 1.318228112 1.789374232 1.854133153   . 1.10085064 1.22825033
## [2,] 0.340258109 0.598988926 0.005719794   . 0.05900444 0.19562976
## [3,] 0.205758979 0.624928389 0.574661104   . 0.96990885 0.31573385
## [4,] 0.129171362 1.149253865 0.091821910   . 0.10878614 0.45618400
## [5,] 1.317402933 1.753933055 1.857993438   . 1.83012744 2.11469960
```

The file at `fpath` follows the specification described [here](https://github.com/ArtifactDB/chihaya).
This provides cross-language portability and ensures that the serialization process is robust to changes in the **DelayedArray** class structure.

## Comments

Many of the basic operations in **DelayedArray** are supported.
However, there are a few operations that are not described by the **chihaya** specification.
An incomplete list is provided below:

- `is.na`.
  This is missing as there is no accepted standard definition of missing-ness.
  (In comparison, `is.nan` is well-defined and is supported by the **chihaya** specification.)
- All distribution functions, e.g., `dpois`, `qunif` and so on.
  These were omitted from the specification as they do not have native implementations in many frameworks.
