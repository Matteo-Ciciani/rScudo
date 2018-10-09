scudoPlot <- function(net, x = "bottomright", y = NULL, ...) {
    plot(net, ...)
    df <- unique(data.frame(igraph::V(net)$color, igraph::V(net)$group,
        stringsAsFactors = FALSE))
    legend(x, y, legend = df[, 2],
        col = df[, 1], pch=c(15,15),
        pt.cex=2)
}
