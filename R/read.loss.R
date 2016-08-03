read.loss <-
function (b, r, data) 
{
    da <- data[[1]]
    rcs <- data[[2]]
    rcateg <- data[[3]]
    bls <- data[[4]]
    blines <- data[[5]]
    x <- da[is.element(da[, 1], bls[bls[, 2] == blines[b], 1]) & 
        is.element(da[, 2], rcs[rcs[, 2] == rcateg[r], 1]), 3:4]
}
