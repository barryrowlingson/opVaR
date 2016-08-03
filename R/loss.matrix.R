loss.matrix <-
function (data) 
{
    da <- data[[1]]
    rcs <- data[[2]]
    rcateg <- data[[3]]
    bls <- data[[4]]
    blines <- data[[5]]
    X = array(NA, c(length(blines), 4, length(rcateg)))
    for (i in 1:length(blines)) {
        for (j in 1:length(rcateg)) {
            a <- read.loss(i, j, data)
            if (length(a[, 1]) > 0) {
                X[i, 1, j] = max(a[, 2])
                X[i, 2, j] = mean(a[, 2])
                X[i, 3, j] = min(a[, 2])
                X[i, 4, j] = length(a[, 1])
            }
            if (length(a[, 1]) == 0) {
                X[i, , j] <- c(0, 0, 0, 0)
            }
        }
    }
    X
}
