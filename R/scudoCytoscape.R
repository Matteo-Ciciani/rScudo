#' @include class.R scudoTrain.R accessors.R
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
#' @param graph object of class \code{\link[igraph:aaa-igraph-package]{igraph}},
#' like the result of \code{\link{scudoNetwork}}
#' @param title the title of the network
#' @param collection the name of the Cytoscape collection
#'
#' @return The network SUID (an integer).
#'
#' @seealso \code{\link{scudoNetwork}}, \code{\link[RCy3]{RCy3}}
#'
#' @author Matteo Ciciani \email{matteo.ciciani@@gmail.com}, Thomas Cantore
#' \email{cantorethomas@@gmail.com}
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
scudoCytoscape <- function(graph, title = "Scudo Graph", collection = "SCUDO") {

    if (!requireNamespace("RCy3", quietly = TRUE)) {
        stop("Package \"RCy3\" needed for this function to work.", "
            Please install it.",  call. = FALSE)
    }

    stopifnot(
        S4vectors::isSingleStirng(title),
        S4Vectors::isSingleString(collection)
    )

    # plot

    id <- RCy3::createNetworkFromIgraph(graph, title = title,
        collection = collection)

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
