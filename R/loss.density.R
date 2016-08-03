loss.density <-
function (a, b, data, no = FALSE, rnumb = c(), bnumb = c(), period = c("none", 
    "days", "weeks", "months", "quarters"), ...) 
{
    if (missing(period)) {
        period <- "none"
    }
    da <- data[[1]]
    rcs <- data[[2]]
    rcateg <- data[[3]]
    bls <- data[[4]]
    blines <- data[[5]]
    if (length(rnumb) == 0) {
        rnumb = c(1:length(rcateg))
    }
    if (length(bnumb) == 0) {
        bnumb = c(1:length(blines))
    }
    if (a == 1) {
        if (no == TRUE) {
            k <- 1
            num <- {
            }
            for (i in bnumb) {
                l <- read.loss(i, b, data)
                if (dim(l)[1] != 0) {
                  num[[k]] <- i
                  k <- k + 1
                }
            }
            bnumb <- num
            if (length(bnumb) == 0) {
                stop("no data to plot!")
            }
        }
        n1 <- ceiling(sqrt(length(bnumb)))
        n2 <- floor(length(bnumb)/n1) + ifelse(length(bnumb)/n1 - 
            floor(length(bnumb)/n1) > 0, 1, 0)
        par(mfrow = c(n1, n2))
        for (i in bnumb) {
            X <- read.loss(i, b, data)[, 2]
            if (all(period != "none" & length(X) != 0)) {
                X <- period.loss(read.loss(i, b, data), period)
            }
            if (length(X) > 0) {
                plot(density(X, ...), ylab = "density", xlab = "loss value", 
                  main = c(paste(blines[i], sep = "/", rcateg[b]), 
                    paste("(", length(X), ")")))
            }
            if (all(length(X) == 0 & no == FALSE)) {
                plot("", ylab = "density", xlab = "loss value", 
                  xlim = c(0, 10), ylim = c(0, 10), main = c(paste(blines[i], 
                    rcateg[b], sep = "/"), paste("(", "0", ")")))
            }
        }
    }
    if (a == 2) {
        if (no == TRUE) {
            k <- 1
            num <- {
            }
            for (j in rnumb) {
                l <- read.loss(b, j, data)
                if (dim(l)[1] != 0) {
                  num[[k]] <- j
                  k <- k + 1
                }
            }
            rnumb <- num
            if (length(rnumb) == 0) {
                stop("no data to plot!")
            }
        }
        n1 <- ceiling(sqrt(length(rnumb)))
        n2 <- floor(length(rnumb)/n1) + ifelse(length(rnumb)/n1 - 
            floor(length(rnumb)/n1) > 0, 1, 0)
        par(mfrow = c(n1, n2))
        for (j in rnumb) {
            X <- read.loss(b, j, data)[, 2]
            if (all(period != "none" & length(X) != 0)) {
                X <- period.loss(read.loss(b, j, data), period)
            }
            if (length(X) > 0) {
                plot(density(X, ...), ylab = "density", xlab = "loss value", 
                  main = c(paste(blines[b], rcateg[j], sep = "/"), 
                    paste("(", length(X), ")")))
            }
            if (all(length(X) == 0 & no == FALSE)) {
                plot("", ylab = "density", xlab = "loss value", 
                  xlim = c(0, 10), ylim = c(0, 10), main = c(paste(blines[b], 
                    rcateg[j], sep = "/"), paste("(", "0", ")")))
            }
        }
    }
}
