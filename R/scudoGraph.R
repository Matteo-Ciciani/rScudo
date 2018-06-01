#' @include class.R scudo.R accessors.R
NULL

# Implement as a method?
#' @export
scudoGraph <- function(scudoResult, N, colors = NULL) {
    # perform some checks

    # get distance matrix and generate adjacency matrix according to N
    adjMatrix <- matrix(0, nrow = dim(DistMatrix(scudoResult))[1],
        ncol = dim(DistMatrix(scudoResult))[1])
    NQuantile <- stats::quantile(DistMatrix(scudoResult)[!.isZero(
        DistMatrix(scudoResult))], probs = N)
    adjMatrix[DistMatrix(scudoResult) <= NQuantile] <- 1
    colnames(adjMatrix) <- colnames(DistMatrix(scudoResult))

    # generate graph using graph_from_adjacency_matrix
    result <- igraph::graph_from_adjacency_matrix(adjMatrix,
        mode = "undirected", diag = FALSE)

    # add weights
    igraph::E(result)$weight <- DistMatrix(scudoResult)[as.logical(adjMatrix) &
        lower.tri(DistMatrix(scudoResult))]

    # add group and color annotation
    igraph::V(result)$group <- Groups(scudoResult)
    if (is.null(colors)) {
        pal <- grDevices::terrain.colors(length(levels(Groups(scudoResult))))
        igraph::V(result)$color <- pal[as.integer(Groups(scudoResult))]
    } else {
        igraph::V(result)$color <- colors
    }

    result
}

# as method?
#' @export
scudo2Cytoscape <- function(scudoIgraph) {
    # perform checks?
    # add more customization?
    # add layout

    RCy3::createNetworkFromIgraph(scudoIgraph, title =
        deparse(substitute(scudoIgraph)), collection = "SCUDO")

    RCy3::setNodeShapeDefault("Ellipse")
    RCy3::lockNodeDimensions(TRUE)
    RCy3::setNodeBorderWidthDefault(1.5)
    RCy3::setNodeColorMapping("color", mapping.type = "p")
    style <- list(visualProperty = "NODE_TRANSPARENCY", value = 200)
    RCy3::setVisualPropertyDefault(style)
    RCy3::setEdgeLineWidthDefault(1.5)
    RCy3::layoutNetwork("cose")

}
