fit.plot <-
function (x, densfun, param, distname = NULL, col = c("red"), 
    col2 = c("grey"), col3 = c("blue"), ylim = c(), xlim = c(), 
    kernel = NULL, n = NULL, draw.diff = F, draw.max = F, scaled = F, 
    positive = T, ...) 
{
    if (!is.null(distname)) {
        if (distname == "beta") {
            scaled <- T
        }
    }
    nm <- names(param)
    f <- formals(densfun)
    args <- names(f)
    m <- match(nm, args)
    formals(densfun) <- c(f[c(1, m)], f[-c(1, m)])
    dens <- function(parm, x, ...) densfun(x, parm, ...)
    if ((l <- length(nm)) > 1) 
        body(dens) <- parse(text = paste("densfun(x,", paste("parm[", 
            1:l, "]", collapse = ", "), ", ...)"))
    dn <- function(x, n, kernel) {
        if (!is.null(n) & !is.null(kernel)) {
            dn <- density(x = x, n = n, kernel = kernel)
        }
        if (!is.null(n) & is.null(kernel)) {
            dn <- density(x = x, n = n)
        }
        if (is.null(n) & !is.null(kernel)) {
            dn <- density(x = x, kernel = kernel)
        }
        if (is.null(n) & is.null(kernel)) {
            dn <- density(x = x)
        }
        dn
    }
    if (scaled == T) {
        maximum <- max(x)
        max1 <- max(dn(x, n, kernel)$y)
        x.new <- x/max(x)
        max2 <- max(dens(x = x.new, as.numeric(param)))
        scale <- max1/max2
    }
    if (length(xlim) == 0 & length(ylim) == 0) {
        plot(dn(x, n, kernel), main = paste("Empirical and fitted density:", 
            distname), ...)
    }
    if (length(xlim) != 0 & length(ylim) == 0) {
        plot(dn(x, n, kernel), xlim = xlim, main = paste("Empirical and fitted density:", 
            distname), ...)
    }
    if (length(xlim) == 0 & length(ylim) != 0) {
        plot(dn(x, n, kernel), ylim = ylim, main = paste("Empirical and fitted density:", 
            distname), ...)
    }
    if (length(xlim) != 0 & length(ylim) != 0) {
        plot(dn(x, n, kernel), ylim = ylim, xlim = xlim, main = paste("Empirical and fitted density:", 
            distname), ...)
    }
    if (scaled == FALSE) {
        curve(dens(x = x, as.numeric(param)), add = TRUE, col = col, 
            lwd = 2)
    }
    if (scaled == TRUE) {
        curve(scale * dens(x = x/maximum, as.numeric(param)), 
            add = TRUE, col = col, lwd = 2)
    }
    xp <- dn(x = x, n, kernel)$x
    if (positive == T) {
        nmbrs <- which(xp > 0)
        xp <- xp[xp > 0]
    }
    if (positive == F) {
        nmbrs <- c(1:length(xp))
    }
    yp <- dn(x = x, n, kernel)$y[nmbrs]
    if (scaled == F) {
        teor <- dens(x = xp, as.numeric(param))
    }
    if (scaled == T) {
        teor <- scale * dens(x = xp/maximum, as.numeric(param))
    }
    emp <- yp
    if (draw.diff == T) {
        for (i in 1:length(xp)) {
            lines(c(xp[i], xp[i]), c(emp[i], teor[i]), col = col2)
        }
    }
    diff <- abs(teor - emp)
    ad <- sum(diff)
    maxdiff <- max(diff)
    meandiff <- mean(diff)
    if (draw.max == T) {
        num <- which(diff == max(diff))
        for (i in num) {
            lines(c(xp[i], xp[i]), c(emp[i], teor[i]), col = col3)
        }
    }
    structure(list(teor = teor, emp = emp, ad = ad, maxdiff = maxdiff, 
        meandiff = meandiff), class = "fitplot")
}
