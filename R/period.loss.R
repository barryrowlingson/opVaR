period.loss <-
function (data, period = c("none", "days", "weeks", "months", 
    "quarters"), dts = FALSE) 
{
    a <- as.Date(data[, 1])
    a <- sort(a)
    if (missing(period)) {
        period <- "none"
    }
    if (period == "none") {
        z <- data[, 2]
        if (dts == TRUE) {
            names <- data[, 1]
        }
    }
    if (period != "none") {
        if (period == "days") {
            y <- as.numeric(table(a))
            if (dts == TRUE) {
                names <- names(table(a))
            }
        }
        if (period == "weeks") {
            week.days <- weekdays(c(0:6) + as.Date("2010-01-04"))
            x <- which(week.days == weekdays(a[1]))
            begin <- a[1] - (x - 1)
            x <- which(week.days == weekdays(a[length(a)]))
            end <- a[length(a)] + (7 - x)
            n <- (as.numeric(difftime(end, begin)) + 1)/7
            y <- table(cut(a, br = begin + 7 * c(0:n)))
            if (dts == TRUE) {
                names <- names(y[y > 0])
            }
            y <- as.numeric(y)
        }
        if (period == "months") {
            a <- as.Date(cut(a, breaks = c("month")))
            b <- as.Date(cut(c(min(a), max(a)), breaks = c("month")))
            c <- seq(b[1], b[2] + 31, by = "month")
            y <- table(cut(a, br = c))
            if (dts == TRUE) {
                names <- names(y[y > 0])
            }
            y <- as.numeric(y)
        }
        if (period == "quarters") {
            a <- as.Date(cut(a, breaks = c("quarter")))
            b <- as.Date(cut(c(min(a), max(a)), breaks = c("quarter")))
            c <- seq(b[1], b[2] + 92, by = "month")
            d <- as.Date(cut(c, breaks = c("quarter")))
            d <- unique(d)
            y <- table(cut(a, br = d))
            if (dts == TRUE) {
                names <- names(y[y > 0])
            }
            y <- as.numeric(y)
        }
        data2 <- data[, 2]
        data2 <- data2[order(a)]
        z <- key.sum(data2, y)
    }
    if (dts == TRUE) {
        names(z) <- names
    }
    z
}
