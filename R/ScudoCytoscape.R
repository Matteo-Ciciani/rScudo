#' @include class.R scudo.R accessors.R
NULL

#' ScudoCytoscape
#'
#' Visualize results of scudo analysis in Cytoscape
#'
#' @param scudoIgraph object of class igraph, like the result of
#'   \code{\link{ScudoNetwork}}.
#' @param title The title of the network. If NULL it defaults to
#'   \code{deparse(substitute(scudoIgraph))}.
#' @param collection The name of the collection.
#' @param base.url See \code{\link[RCy3]{createNetworkFromIgraph}}
#'
#' @return The network SUID (an integer).
#'
#' @export
ScudoCytoscape <- function(scudoIgraph, title = NULL, collection = "SCUDO",
                           base.url = NULL) {
    # perform checks
    if (is.null(title)) title <- deparse(substitute(scudoIgraph))

    stopifnot(
        is.character(title),
        is.character(collection)
    )

    if (length(title) != 1) stop("title length must be 1")
    if (length(collection) != 1) stop("collection length must be 1")

    # plot
    if (is.null(base.url)) {
        id <- RCy3::createNetworkFromIgraph(scudoIgraph, title = title,
            collection = collection)
    } else {
        id <- RCy3::createNetworkFromIgraph(scudoIgraph, title = title,
            collection = collection, base.url = base.url)
    }
    RCy3::setNodeShapeDefault("Ellipse")
    RCy3::lockNodeDimensions(TRUE)
    RCy3::setNodeBorderWidthDefault(1.5)
    RCy3::setNodeColorMapping("color", mapping.type = "p")
    style <- list(visualProperty = "NODE_TRANSPARENCY", value = 200)
    RCy3::setVisualPropertyDefault(style)
    RCy3::setEdgeLineWidthDefault(1.5)
    RCy3::layoutNetwork("cose")
    id
}
