#' @include class.R scudo.R accessors.R
NULL

#' Create a Cytoscape network from the output of scudoNetwork
#'
#' A wrapper to \code{\link[RCy3]{RCy3}} function calls to plot the result of
#' \code{\link{scudoNetwork}} in Cytoscape 3. Cytoscape must be open before
#' running this function.
#'
#' @usage scudoCytoscape(graph, title = NULL, collection = "SCUDO",
#'     base.url = NULL)
#'
#' @param graph object of class \code{\link[igraph:igraph-package]{igraph}},
#' like the result of \code{\link{scudoNetwork}}
#' @param title the title of the network. If NULL it defaults to
#'   \code{deparse(substitute(graph))}
#' @param collection the name of the Cytoscape collection
#' @param base.url see \code{\link[RCy3]{createNetworkFromIgraph}}
#'
#' @return The network SUID (an integer).
#'
#' @seealso \code{\link{scudoNetwork}}, \code{\link[RCy3]{RCy3}}
#'
#' @author Matteo Ciciani \email{matteo.ciciani@@gmail.com}
#'
#' @examples
#' # generate dummy dataset
#' exprData <- data.frame(a = 11:20, b = 16:25,
#'     c = rev(1:10), d = c(1:2, rev(3:10)))
#' rownames(exprData) <- letters[11:20]
#' grps <- as.factor(c("G1", "G1", "G2", "G2"))
#' nTop <- 2
#' nBottom <- 3
#'
#' # run scudoTrain and scudoNetwork
#' res <- scudoTrain(exprData, grps, nTop, nBottom, foldChange = FALSE,
#'     featureSel = FALSE)
#' col <- c("#FF00FF", "#FF00FF", "#00FF00", "#00FF00")
#' net <- scudoNetwork(res, N = 0.5, colors = col)
#'
#' # run scudoCytoscape (with Cytoscape open)
#' \dontrun{scudoCytoscape(res, title = "scudoCytoscape output")}
#'
#' @export
scudoCytoscape <- function(graph, title = NULL, collection = "SCUDO",
    base.url = NULL) {

    if (!requireNamespace("RCy3", quietly = TRUE)) {
        stop("Package \"RCy3\" needed for this function to work.", "
            Please install it.",  call. = FALSE)
    }

    # perform checks
    if (is.null(title)) title <- deparse(substitute(graph))

    stopifnot(
        is.character(title),
        is.character(collection)
    )

    if (length(title) != 1) stop("title length must be 1")
    if (length(collection) != 1) stop("collection length must be 1")

    # plot
    if (is.null(base.url)) {
        id <- RCy3::createNetworkFromIgraph(graph, title = title,
            collection = collection)
    } else {
        id <- RCy3::createNetworkFromIgraph(graph, title = title,
            collection = collection, base.url = base.url)
    }
    RCy3::setNodeShapeDefault("Ellipse")
    RCy3::lockNodeDimensions(TRUE)
    RCy3::setNodeBorderWidthDefault(1.5)
    RCy3::setNodeColorMapping("color", mapping.type = "p")
    style <- list(visualProperty = "NODE_TRANSPARENCY", value = 200)
    RCy3::setVisualPropertyDefault(style)
    RCy3::setEdgeLineWidthDefault(1)
    RCy3::layoutNetwork("cose")
    id
}
