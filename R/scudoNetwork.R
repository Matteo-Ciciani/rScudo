#' @include class.R scudoTrain.R accessors.R utilities.R
NULL

#' Create graph from a scudoResults object
#'
#' A function to create an \code{\link[igraph:igraph-package]{igraph}} object
#' from a \code{\linkS4class{scudoResults}} object. In the graph, nodes are
#' samples and edges quantify the similarity between the nodes.
#'
#' This function uses the distance matrix in the
#' \code{\linkS4class{scudoResults}} object to generates an
#' \code{\link[igraph:igraph-package]{igraph}} object, representing a graph
#' where nodes are samples and edges quantify the similarity between the
#' signatures of pairs of nodes.
#'
#' The distance matrix in \code{object} is used to generate an unweighted
#' adjacency matrix, that is then used to generate the graph. The sample
#' quantile of probability N, computed from all the non-zero distances in the
#' distance matrix, is used as a threshold to generate the adjacency matrix: all
#' the distances larger than this quantile are mapped to 0, all the distances
#' smaller than this quantile are mapped to 1 (with the exception of the
#' distances of any node from itself, which are equal to 0).
#'
#' Distances are set as attributes of the edges. Use \code{
#' igraph::E(igraphObject)$distance} to retrieve them, where
#' \code{igraphObject} is the result of \code{scudoNetwork}.
#'
#' The \code{color} parameter controls the color of the nodes. It must be a
#' vector of hexadecimal RGB color codes (like \code{"#FFFFFF"}), with length
#' equal to
#' the number of samples in \code{object}. By default, a different color is
#' assigned to each group. If no group is specified in \code{object}, all nodes
#' are set to the same color. A vector of node colors can be accessed
#' with \code{ igraph::V(igraphObject)$color}. Use
#' \code{igraph::V(igraphObject)$group} to access the group label of each node
#' (it returns NULL if no group is specified in \code{object}).
#'
#' @param object a \code{\linkS4class{scudoResults}} object
#' @param N a number between 0 and 1, representing the fraction of the
#' signature-to-signature distances that will be used to draw the graph
#' @param colors a character vector of hexadecimal RGB color codes used to color
#' the nodes of the graph. \code{length(colors)} must be equal to the number of
#' samples. By default colors are chosen according to the groups in
#' \code{object}
#'
#' @return An object of class \code{\link[igraph:igraph-package]{igraph}}.
#'
#' @seealso \code{\link{scudoCytoscape}}, \code{\linkS4class{scudoResults}},
#' \code{\link[igraph:igraph-package]{igraph}}
#'
#' @author Matteo Ciciani \email{matteo.ciciani@@gmail.com}, Thomas Cantore
#' \email{cantorethomas@@gmail.com}
#'
#' @examples
#' # generate dummy dataset and run scudo
#' exprData <- data.frame(a = 11:20, b = 16:25,
#'     c = rev(1:10), d = c(1:2, rev(3:10)))
#' rownames(exprData) <- letters[11:20]
#' grps <- as.factor(c("G1", "G1", "G2", "G2"))
#' nTop <- 2
#' nBottom <- 3
#'
#' res <- scudoTrain(exprData, grps, nTop, nBottom, foldChange = FALSE,
#'     featureSel = FALSE)
#'
#' # generate network
#' col <- c("#FF0000", "#FF0000", "#0000FF", "#0000FF")
#' net <- scudoNetwork(res, N = 0.5, colors = col)
#'
#' # retrieve node colors and groups
#' nodes <- igraph::V(net)
#' colors <- nodes$color
#' groups <- nodes$group
#'
#' # retrieve distances from edges
#' edges <- igraph::E(net)
#' dist <- edges$distance
#'
#' # plot the network
#' scudoPlot(net)
#'
#' @rdname scudoNetwork-methods
#' @export
setGeneric("scudoNetwork", function(object, N, colors = character())
    standardGeneric("scudoNetwork"))

#' @rdname scudoNetwork-methods
#' @aliases scudoNetwork,scudoResults-method
#' @usage NULL
setMethod("scudoNetwork", signature = "scudoResults", definition =
    function(object, N, colors) {

        # input checks
        stopifnot(
            is.numeric(N),
            is.vector(N),
            length(N) == 1,
            N > 0,
            N <= 1.0,
            is.character(colors),
            is.vector(colors)
        )

        if (length(colors) != 0) {
            if (any(is.na(colors))) stop("colors contains NAs")
            if (length(colors) != dim(distMatrix(object))[1]) {
                stop(paste("length of colors differs from number of samples",
                    "in object"))
            }
            if (any(is.na(stringr::str_match(colors, "^#[0-9a-fA-F]{6,8}$")))) {
                stop(paste("colors contains invalid hexadecimal colors (see",
                "documentation for correct format)"))
            }
        }

        # get distance matrix and generate igraph object
        result <- .makeNetwork(distMatrix(object), N)

        # add group and color annotation
        if (length(groupsAnnotation(object)) == 0) {
            igraph::V(result)$color <- rep("#FFFFFF",
                dim(distMatrix(object))[1])
        } else {
            igraph::V(result)$group <- as.character(groupsAnnotation(object))

            if (length(colors) == 0) {
                pal <- grDevices::rainbow(length(levels(groupsAnnotation(
                    object))))
                pal <- stringr::str_extract(pal, "^#[0-9a-fA-F]{6}")
                igraph::V(result)$color <- pal[as.integer(groupsAnnotation(
                    object))]
            } else {
                igraph::V(result)$color <- stringr::str_extract(colors,
                    "^#[0-9a-fA-F]{6}")
            }
        }

        result
    }
)
