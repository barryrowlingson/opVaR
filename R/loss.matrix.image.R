loss.matrix.image <-
function (D, businesslines, riskcategories, data = NULL, col1 = c("lavender"), 
    col2 = c("rosybrown 1"), col3 = c("lightpink"), col4 = c("mistyrose"), 
    col5 = c("mintcream"), cex1 = 0.6, cex2 = 0.6, cex3 = 0.6) 
{
    if (missing(D) & missing(businesslines) & missing(riskcategories) & 
        !is.null(data)) {
        D <- loss.matrix(data)
        businesslines <- data$blines
        riskcategories <- data$rcateg
    }
    plot("", type = "l", lwd = 2, axes = FALSE, ylab = "risk category", 
        xlab = "business line", ylim = c(-2, length(riskcategories) + 
            2), xlim = c(-2, length(businesslines)))
    for (i in 0:length(riskcategories)) {
        lines(c(0, length(businesslines)), c(i, i))
    }
    for (i in 0:length(businesslines)) {
        lines(c(i, i), c(0, length(riskcategories)))
    }
    rect(length(businesslines) - 1.5, length(riskcategories) + 
        1, length(businesslines) - 0.5, length(riskcategories) + 
        2, col = c(col5))
    text(length(businesslines) - 1.5, length(riskcategories) + 
        1.5, "mean loss", cex = cex3, pos = 4)
    text(length(businesslines) - 1.5, length(riskcategories) + 
        1.8, "number of losses", cex = cex3, pos = 4)
    text(length(businesslines) - 1.5, length(riskcategories) + 
        1.2, "max loss", cex = cex3, pos = 4)
    for (i in 1:length(riskcategories)) {
        text(-0.1, c(i - 0.5), riskcategories[i], cex = cex1, 
            pos = 2)
    }
    for (i in 1:length(businesslines)) {
        text(c(i - 0.5), -0.1, businesslines[i], srt = 90, cex = cex1, 
            pos = 2)
    }
    for (i in 1:length(businesslines)) {
        for (j in 1:length(riskcategories)) {
            if (D[i, 2, j] == 0) {
                rect(i - 1, j - 1, i, j, col = c(col1))
            }
            if (D[i, 2, j] >= mean(as.vector(D[, 2, ]))) {
                rect(i - 1, j - 1, i, j, col = c(col2))
            }
            if (D[i, 2, j] >= mean(as.vector(D[, 2, ])) + sqrt(var(as.vector(D[, 
                2, ])))) {
                rect(i - 1, j - 1, i, j, col = c(col3))
            }
            if (D[i, 2, j] > 0 & D[i, 2, j] < mean(as.vector(D[, 
                2, ]))) {
                rect(i - 1, j - 1, i, j, col = c(col4))
            }
        }
    }
    for (i in 1:length(businesslines)) {
        for (j in 1:length(riskcategories)) {
            if (D[i, 4, j] == 0) {
                text(i - 0.5, j - 0.5, round(D[i, 4, j], 4), 
                  cex = cex2)
            }
            if (D[i, 4, j] != 0) {
                text(i - 0.7, j - 0.6, round(D[i, 2, j], 4), 
                  cex = cex2, adj = c(0, 0))
                text(i - 0.7, j - 0.8, round(D[i, 1, j], 4), 
                  cex = cex2, adj = c(0))
                text(i - 0.5, j - 0.2, round(D[i, 4, j], 4), 
                  cex = cex2)
            }
        }
    }
}
