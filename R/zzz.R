known.env <- new.env()

known.env$operations <- list(
    subset=.load_delayed_subset,
    transpose=.load_delayed_aperm,
    combine=.load_delayed_combine,
    `binary arithmetic`=.load_delayed_nary_iso,
    `binary comparison`=.load_delayed_nary_iso,
    `binary logic`=.load_delayed_nary_iso,
    dimnames=.load_delayed_dimnames,
    `subset assignment`=.load_delayed_subassign,
    `unary arithmetic`=.load_delayed_unary_iso,
    `unary comparison`=.load_delayed_unary_iso,
    `unary logic`=.load_delayed_unary_iso,
    `unary math`=.load_delayed_unary_iso,
    `unary special check`=.load_delayed_unary_iso,
    `matrix product`=.load_matrix_product,
    `residual matrix`=.load_residual_matrix
)

known.env$arrays <- list(
    `dense array`=.load_array,
    `sparse matrix`=.load_csparse_matrix,
    `external hdf5 dense array`=.load_dense_hdf5_array,
    `external hdf5 sparse matrix`=.load_sparse_hdf5_matrix
)
