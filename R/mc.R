mc <-
function (x, rfun = NULL, period, iterate, nmb = 1000, begin = NULL, 
    end = NULL, wknd = TRUE, crt = 0, type = NULL, param = NULL, 
    zero = F, distname = NULL, fit = T, flist = c("beta", "cauchy", 
        "chi-squared", "exponential", "f", "gamma", "log-normal", 
        "logistic", "normal", "weibull", "inverse gaussian"), 
    p = c(0.95, 0.99, 0.999), ...) 
{
    if (missing(period)) {
        period <- "days"
    }
    if (missing(iterate)) {
        iterate <- "years"
    }
    t <- switch(EXPR = period, days = ifelse(wknd == T, 365, 
        252), weeks = 52, months = 12, quarters = 4, NULL)
    iterate <- switch(EXPR = iterate, years = t * nmb, quarters = t * 
        nmb * 1/4, months = t * nmb * 1/12, weeks = t * nmb * 
        1/52, days = t * nmb * 1/ifelse(wknd == T, 365, 252), 
        NULL)
    k <- iterate * t * nmb/365
    if (k - floor(k) > 0) {
        print("note that these are not full years")
    }
    if (is.null(type)) {
        u <- {
        }
        u[1] <- as.numeric(root.period(x, period, "poisson", 
            wknd = wknd, crt = crt, begin = begin, end = end, 
            ...)$p)
        u[2] <- as.numeric(root.period(x, period, "binomial", 
            wknd = wknd, crt = crt, begin = begin, end = end, 
            ...)$p)
        u[3] <- as.numeric(root.period(x, period, "nbinomial", 
            wknd = wknd, crt = crt, begin = begin, end = end, 
            ...)$p)
        names(u) <- c("poisson", "binomial", "nbinomial")
        type <- names(which(u == max(u))[1])
        print(u[which(u == max(u))])
    }
    parameters <- as.list(root.period(x, period, as.character(type), 
        wknd = wknd, crt = crt, begin = begin, end = end, ...)$param)
    print(parameters)
    param1 <- as.numeric(parameters)
    if (type == "poisson") {
        n <- rpois(iterate, param1[1])
    }
    if (type == "binomial") {
        n <- rbinom(iterate, param1[2], param1[1])
    }
    if (type == "nbinomial") {
        n <- rnbinom(iterate, param1[1], param1[2])
    }
    ad <- NULL
    if (is.null(rfun)) {
        u <- {
        }
        for (i in flist) {
            u[i] <- loss.fit.dist(x, densfun = i, period = period)$ad
        }
        rfun <- names(which(u == min(u))[1])
        param <- loss.fit.dist(x, period = period, densfun = flist[which(u == 
            min(u))[1]])$param
        ad <- loss.fit.dist(x, period = period, densfun = flist[which(u == 
            min(u))[1]])$ad
        print(rfun)
    }
    if (is.character(rfun)) {
        if (fit == T) {
            if (is.element(rfun, flist)) {
                param <- loss.fit.dist(x, period = period, densfun = rfun)$param
                ad <- loss.fit.dist(x, period = period, densfun = rfun)$ad
            }
        }
    }
    if (is.character(rfun)) {
        distname <- tolower(rfun)
        rfun <- switch(EXPR = rfun, beta = rbeta, cauchy = rcauchy, 
            `chi-squared` = rchisq, exponential = rexp, f = rf, 
            gamma = rgamma, `log-normal` = rlnorm, lognormal = rlnorm, 
            logistic = rlogis, normal = rnorm, weibull = rweibull, 
            `inverse gaussian` = rinvGauss, NULL)
    }
    print(param)
    nm <- names(param)
    f <- formals(rfun)
    args <- names(f)
    m <- match(nm, args)
    formals(rfun) <- c(f[c(1, m)], f[-c(1, m)])
    dens <- function(parm, n, ...) rfun(n, parm, ...)
    if ((l <- length(nm)) > 1) 
        body(dens) <- parse(text = paste("rfun(n,", paste("parm[", 
            1:l, "]", collapse = ", "), ", ...)"))
    r <- rep(0, length(n))
    for (i in 1:length(n)) {
        if (n[i] == 0) {
            r[i] <- 0
        }
        if (n[i] > 0) {
            r[i] <- sum(dens(n = n[i], as.numeric(param)))
        }
    }
    if (all(!is.null(distname) & distname == "beta")) {
        r <- r * max(x[, 2])
    }
    y <- matrix(r, t)
    r <- apply(y, 2, sum)
    q <- quantile(r, p)
    plot(ecdf(r), main = c("ecdf(losses)"))
    for (i in 1:length(q)) {
        lines(c(q[i], q[i]), c(0, 1.5), col = "red")
        text(q[i], 0.05, cex = 0.9, paste(round(q[i])), pos = 2)
        text(q[i], 0.1, cex = 0.9, paste(names(q[i])), pos = 2)
    }
    table = list(losses = r, q = q, ad = ad)
    structure(list(table = table))
}
