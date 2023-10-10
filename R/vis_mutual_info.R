## ----
# Plot mutual information from a `PHGDataSet` object
#
# @param phgObj A `PHGDataSet` object
plotMutualInfoFromPHGDataSet <- function(phgObj) {
    m      <- calcMutualInfo(phgObj)
    xy     <- t(utils::combn(colnames(m), 2))
    mDf    <- data.frame(xy, mut = m[xy])
    mDf$X1 <- as.factor(as.numeric(gsub("R", "", mDf$X1)))
    mDf$X2 <- as.factor(as.numeric(gsub("R", "", mDf$X2)))

    p <- ggplot2::ggplot(mDf) +
        ggplot2::aes(x = X1, y = X2, fill = mut) +
        ggplot2::geom_tile(
            color = "white",
            lwd = 0.5,
            linetype = 1
        ) +
        ggplot2::labs(fill = "Mutuality") +
        ggplot2::theme(axis.title = ggplot2::element_blank()) +
        ggplot2::coord_equal()

    return(p)
}


