loss.fit.dist <-
function (densfun, x, start = NULL, name = NULL, qq = FALSE, 
    period, ylim = c(), xlim = c(), col = "red", from = 0.1^15, 
    to = 1 - 0.1^15, length.out = 10000, by = NULL, kernel = NULL, 
    n = NULL, draw.diff = F, draw.max = F, xlog.scale = F, ...) 
{
    y <- match.call()[[2]]
    if (missing(period)) {
        period <- "none"
    }
    if (!is.element(period, c("none", "days", "weeks", "months", 
        "quarters"))) {
        stop("period should be none, days, weeks, months or quarters")
    }
    if (!is.null(dim(x))) {
        if (dim(x)[2] != 2) 
            stop("x should be one- or two dimensional")
        if (period != "none") {
            x <- period.loss(x, period)
        }
        if (period == "none") {
            x <- x[, 2]
        }
        if (length(dim(x)) > 2) 
            stop("x should be one- or two dimensional")
    }
    distname <- NULL
    qfun <- NULL
    k <- NULL
    if (is.character(densfun)) {
        distname <- tolower(densfun)
        k <- switch(EXPR = densfun, beta = "qbeta", cauchy = "qcauchy", 
            `chi-squared` = "qchisq", exponential = "qexp", f = "qf", 
            gamma = "qgamma", `log-normal` = "qlnorm", lognormal = "qlnorm", 
            logistic = "qlogis", normal = "qnorm", weibull = "qweibull", 
            `inverse gaussian` = "qinvGauss", NULL)
        densfun <- switch(EXPR = densfun, beta = dbeta, cauchy = dcauchy, 
            `chi-squared` = dchisq, exponential = dexp, f = df, 
            gamma = dgamma, `log-normal` = dlnorm, lognormal = dlnorm, 
            logistic = dlogis, normal = dnorm, weibull = dweibull, 
            `inverse gaussian` = dinvGauss, NULL)
    }
    if (is.null(densfun)) {
        stop("unsupported distribution")
    }
    m <- mean(x)
    v <- var(x)
    sd <- sd(x)
    if (!is.null(distname)) {
        if (distname == "beta" & is.null(start)) {
            x.old <- x
            x <- x/max(x)
            print("Argument scaled; x<- x/max(x)")
            scaled = TRUE
            m <- mean(x)
            v <- var(x)
            x <- x[x < 1]
            start <- list(shape1 = max(-(m * (m^2 - m + v))/v, 
                0.1^(100)), shape2 = max((-1 + m) * (-m + m^2 + 
                v)/v, 0.1^(100)))
        }
        if (distname == "gamma" & is.null(start)) {
            start <- list(shape = max(m^2/v, 0), scale = max(v/m, 
                0.1^(100)))
        }
        if (distname == "inverse gaussian" & is.null(start)) {
            start <- list(lambda = max(m^3/v, 0.1^(100)), nu = max(m, 
                0.1^(100)))
        }
        if (distname == "chi-squared") {
            start <- list(df = m)
        }
        if (distname == "f") {
            start <- list(df1 = max(-2 * m^2/(-m^2 + m^3 - 2 * 
                v + m * v), 4), df2 = max(2 * m/(m - 1), 2))
        }
        if (!is.null(start) & distname %in% c("lognormal", "log-normal", 
            "exponential", "normal")) {
            stop(paste(" supplying pars for the ", distname, 
                " is not supported"))
        }
        if (is.null(start) & !(distname %in% c("inverse gaussian"))) {
            param <- as.list(fitdistr(x, distname, ...)$estimate)
            loglik <- fitdistr(x, distname, ...)$loglik
            ese <- fitdistr(x, distname, ...)$sd
        }
        if (!is.null(start) & !distname %in% c("inverse gaussian", 
            "f")) {
            param <- as.list(fitdistr(x, distname, start = start, 
                ...)$estimate)
            loglik <- fitdistr(x, distname, start = start, ...)$loglik
            ese <- fitdistr(x, distname, start = start, ...)$sd
        }
        if (distname %in% c("inverse gaussian")) {
            param <- as.list(fitdistr(x, densfun, start = start, 
                ...)$estimate)
            loglik <- fitdistr(x, densfun, start = start, ...)$loglik
            ese <- fitdistr(x, densfun, start = start, ...)$sd
        }
        if (distname %in% c("f")) {
            param <- as.list(fitdistr(x, densfun, start = start, 
                lower = 0.01, ...)$estimate)
            loglik <- fitdistr(x, densfun, start = start, lower = 0.01, 
                ...)$loglik
            ese <- fitdistr(x, densfun, start = start, lower = 0.01, 
                ...)$sd
        }
    }
    if (is.null(start) & is.null(distname)) {
        stop("'start' must be a named list")
    }
    if (!is.null(start) & is.null(distname)) {
        param <- as.list(fitdistr(x, densfun, start = start, 
            ...)$estimate)
        loglik <- fitdistr(x, densfun, start = start, ...)$loglik
        ese <- fitdistr(x, densfun, start = start, ...)$sd
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
    max <- max(dens(x = x, as.numeric(param)))
    min <- min(dens(x = x, as.numeric(param)))
    log.scale <- if (!is.null(distname)) {
        if (distname == "beta") {
            out <- fit.plot(densfun = densfun, x = x.old, param = param, 
                distname = "beta", col = col, ylim = ylim, xlim = xlim, 
                kernel = kernel, n = n, draw.diff = draw.diff, 
                draw.max = draw.max, log = ifelse(xlog.scale == 
                  F, paste(""), paste("x")))
        }
        if (distname != "beta") {
            out <- fit.plot(densfun = densfun, x = x, param = param, 
                distname = distname, col = col, ylim = ylim, 
                xlim = xlim, kernel = kernel, n = n, draw.diff = draw.diff, 
                draw.max = draw.max, log = ifelse(xlog.scale == 
                  F, paste(""), paste("x")))
        }
    }
    if (is.null(distname)) {
        out <- fit.plot(densfun = densfun, x = x, param = param, 
            distname = NULL, col = col, ylim = ylim, xlim = xlim, 
            kernel = kernel, n = n, draw.diff = draw.diff, draw.max = draw.max, 
            log = ifelse(xlog.scale == F, paste(""), paste("x")))
    }
    ad <- out$ad
    names(ad) <- c("ad")
    teor.dens <- out$teor
    emp.dens <- out$emp
    maxdiff <- out$maxdiff
    meandiff <- out$meandiff
    if (qq == TRUE) {
        y <- as.character(y)
        ifelse(is.null(k), k <- paste("q", substr(y, 2, nchar(as.character(y))), 
            sep = ""), k)
        if (is.null(qfun)) {
            qfun <- get(k, mode = "function", envir = parent.frame())
        }
        f <- formals(paste(k))
        args <- names(f)
        m <- match(nm, args)
        formals(qfun) <- c(f[c(1, m)], f[-c(1, m)])
        quan <- function(parm, p, ...) qfun(p, parm, ...)
        if ((l <- length(nm)) > 1) 
            body(quan) <- parse(text = paste("qfun(p,", paste("parm[", 
                1:l, "]", collapse = ", "), ", ...)"))
        if (is.null(by)) {
            q.t <- quan(p = seq(from = from, to = to, length.out = length.out), 
                as.numeric(param))
            q.e <- quantile(x, seq(from = from, to = to, length.out = length.out))
        }
        else {
            q.t <- quan(p = seq(from = from, to = to, by = by), 
                as.numeric(param))
            q.e <- quantile(x, seq(from = from, to = to, by = by))
        }
        qqplot(q.t, q.e, main = paste("QQ-plot distr.", if (!is.null(distname)) 
            distname, if (!is.null(name)) 
            name), xlim = c(q.t[2], q.t[length(q.t) - 1]), ylim = c(q.e[1], 
            q.e[length(q.e)]))
        abline(0, 1)
    }
    if (qq == FALSE) {
        q.e <- NULL
        q.t <- NULL
    }
    structure(list(loglik = loglik, param = param, sd = ese, 
        q.t = q.t, q.e = q.e, ad = ad, teor.dens = teor.dens, 
        emp.dens = emp.dens, maxdiff = maxdiff, meandiff = meandiff), 
        class = "lf")
}
