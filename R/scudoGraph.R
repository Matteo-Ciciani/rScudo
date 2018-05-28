#' @include class.R scudo.R accessors.R
NULL

# Implement as a method?
scudoGraph <- function(scudoResult, N, colors = NULL) {
    # perform some checks

    # get distance matrix and generate adjacency matrix according to N
    adjMatrix <- DistMatrix(scudoResult)
    nonZeroIndeces <- sapply(adjMatrix, function(x)
        !isTRUE(all.equal(x, 0)))
    NQuantile <- stats::quantile(adjMatrix[nonZeroIndeces], probs = N)
    adjMatrix[adjMatrix < NQuantile] <- 0

    # generate graph using graph_from_adjacency_matrix
    result <- igraph::graph_from_adjacency_matrix(adjMatrix,
        mode = "undirected", weighted = TRUE)

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
scudo2Cytoscape <- function(scudoIgraph) {
    # perform checks?
    # add more customization?

    RCy3::createNetworkFromIgraph(scudoIgraph)
    RCy3::setNodeColorMapping("color", mapping.type = "p")
}
