hist.period <-
function (data, period, wknd = TRUE, crt = 0, begin = NULL, end = NULL, 
    ...) 
{
    if (missing(period)) {
        period <- "days"
    }
    data <- data[, 1]
    a <- as.Date(data)
    a <- sort(a)
    if (is.null(begin)) {
        begin <- min(a)
    }
    if (is.null(end)) {
        end <- max(a)
    }
    if (period == "days") {
        x <- as.numeric(difftime(end, begin)) + 1
        y <- as.numeric(table(a))
        if (wknd == T) {
            zero <- x - as.numeric(length(y))
        }
        if (wknd == F) {
            zero <- x - as.numeric(length(y) + 2 * floor(x/7))
        }
        if (crt != 0) {
            zero <- x - as.numeric(length(y) + 2 * floor(x/7) + 
                crt)
        }
        z <- table(c(y, rep(0, zero)))
    }
    if (period == "weeks") {
        week.days <- weekdays(c(0:6) + as.Date("2010-01-04"))
        x <- which(week.days == weekdays(as.Date(begin)))
        begin <- as.Date(begin) - (x - 1)
        x <- which(week.days == weekdays(as.Date(end)))
        end <- as.Date(end) + (7 - x)
        n <- (as.numeric(difftime(end, begin)) + 1)/7
        y <- as.numeric(table(cut(a, br = begin + 7 * c(0:n))))
        z <- table(y)
    }
    if (period == "months") {
        a <- as.Date(cut(a, breaks = c("month")))
        b <- as.Date(cut(as.Date(c(begin, end)), breaks = c("month")))
        c <- seq(b[1], b[2] + 31, by = "month")
        y <- as.numeric(table(cut(a, br = c)))
        z <- table(y)
    }
    if (period == "quarters") {
        a <- as.Date(cut(a, breaks = c("quarter")))
        b <- as.Date(cut(as.Date(c(begin, end)), breaks = c("quarter")))
        c <- seq(b[1], b[2] + 92, by = "month")
        d <- as.Date(cut(c, breaks = c("quarter")))
        d <- unique(d)
        y <- as.numeric(table(cut(a, br = d)))
        z <- table(y)
    }
    barplot(z, main = c(paste("Frequency for", period)), ...)
    structure = list(y = z)
}
