rtriangle <-
function (n = 1, a = 0, b = 1, c = 0.5) 
{
    if (length(n) > 1) 
        n <- length(n)
    if (n < 1 | is.na(n)) 
        stop(paste("invalid argument: n =", n))
    n <- floor(n)
    if (any(is.na(c(a, b, c)))) 
        return(rep(NaN, times = n))
    if (a > c | b < c) 
        return(rep(NaN, times = n))
    if (any(is.infinite(c(a, b, c)))) 
        return(rep(NaN, times = n))
    p <- runif(n)
    if (a != c) {
        i <- which((a + sqrt(p * (b - a) * (c - a))) <= c)
        j <- which((b - sqrt((1 - p) * (b - a) * (b - c))) > 
            c)
    }
    else {
        i <- which((a + sqrt(p * (b - a) * (c - a))) < c)
        j <- which((b - sqrt((1 - p) * (b - a) * (b - c))) >= 
            c)
    }
    if (length(i) != 0) 
        p[i] <- a + sqrt(p[i] * (b - a) * (c - a))
    if (length(j) != 0) 
        p[j] <- b - sqrt((1 - p[j]) * (b - a) * (b - c))
    return(p)
}
