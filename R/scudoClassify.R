#' @include class.R accessors.R utilities.R
NULL

#' Performes classification using SCUDO
#'
#' Placeholder
#'
#'
#'
#'
#'
#'
#' @export
scudoClassify <- function(trainExpData, testExpData, N, nTop, nBottom,
                          trainGroups, testGroups = NULL, alpha = 0.1,
                          norm = TRUE, groupedNorm = FALSE, featureSel = TRUE,
                          parametric = FALSE, pAdj = "none", distFun = NULL,
                          neighbours = 1, weighted = TRUE, pruned = FALSE) {

    # InputCheck ---------------------------------------------------------------
    # perform input checks

    # normalization ------------------------------------------------------------

    trainGroups <- trainGroups[, drop = TRUE]
    testGroups <- testGroups[, drop = TRUE]
    trainNormGroups <- if (groupedNorm) trainGroups else NULL
    testNormGroups <- if (groupedNorm) testGroups else NULL

    if (norm) {
        trainExpData <- .normalization(trainExpData, trainNormGroups)
        testExpData <- .normalization(testExpData, testNormGroups)
    }

    # Feature Selection --------------------------------------------------------
    # training set

    nGroupsTrain <- length(levels(trainGroups))

    if (nGroupsTrain == 1) {
        warning(paste0("Just one group in ", deparse(substitute(groups)),
                       ": skipping feature selection"))
        featureSel <- FALSE
    }

    if (featureSel) {
        trainExpData <- .featureSelection(trainExpData, alpha, trainGroups,
                                          nGroupsTrain, parametric, pAdj)
        if ((nTop + nBottom) > dim(trainExpData)[1]) {
            stop("top and bottom signatures overlap, only ",
                 dim(trainExpData)[1], " features selected.")
        }
    }

    # testing set

    present <- rownames(trainExpData) %in% rownames(testExpData)
    missing <- rownames(trainExpData)[!present]

    if (length(missing) != 0) {
        stop(paste(length(missing), "features present in trainExpData are",
                      "absent in testExpData:\n"))
    }

    testExpData <- testExpData[rownames(trainExpData)[present], ]

    # Performing Scudo on training set -----------------------------------------

    trainScudoRes <- .performScudo(trainExpData, trainGroups, nTop, nBottom,
                                   distFun, alpha, norm, groupedNorm,
                                   featureSel, parametric, pAdj)

    # compute whole distance matrix, then select submatrix with distances from
    # samples in the testing set (columns) from the samples in the training set
    # (rows)

    if (is.null(distFun)) distFun <- .defaultDist

    # classification: if unweighted count, for each vector, the number of nodes
    # for each group that are connected and pass the N threshold; if weighted:
    # if pruned exclude nodes based on N, if not only compute mean weight and
    # perform test (t or w), issue classification and p-value. Possibly
    # consider different number of neighbours

    # if weighted unpruned: only consider first neighbours, compute using the
    #                       disance matrix
    # if weighted pruned: consider neighbours at distance d, recompute the graph
    #                     with each neighbour using N, then bfs to find
    #                     distances
    # if unweighetd: similar to weighted pruned, but use counts instrtead of
    #                weights


    if (weighted) {
        if (pruned) {
            # need a bunch of new functions: take train, add a test sample one
            # one at a time, compute n igraph objects, with n = # test samples,
            # run bfs on each, use distance d to compute group scores for each
            # group. A similar function can be used for unweighed

        } else {
            # compute all-to-all distance matrix
            distMat <- distFun(cbind(trainExpData, testExpData), nTop, nBottom)
            distMat <- distMat[1:dim(trainExpData)[1],
                (dim(trainExpData)[2] + 1):(dim(trainExpData)[2]
                + dim(testExpData)[2])]

            # get sums for each new sample
            distSums <- stats::aggregate(distMat, by = list(trainGroups),
                                         FUN = sum)

        }
    } else {

    }

    # compute signatures
}

.computeTestNetwork <- function(dMatrix, N, trainGroups) {
    # compute adjacency matrix
    adjMatrix <- matrix(0, nrow = dim(dMatrix)[1], ncol = dim(dMatrix)[1])
    NQuantile <- stats::quantile(dMatrix[dMatrix > sqrt(.Machine$double.eps)],
        probs = N)
    adjMatrix[dMatrix <= NQuantile] <- 1
    colnames(adjMatrix) <- colnames(dMatrix)

    # generate graph using graph_from_adjacency_matrix
    result <- igraph::graph_from_adjacency_matrix(adjMatrix,
        mode = "undirected", diag = FALSE)

    # add distances
    igraph::E(result)$distance <- dMatrix[as.logical(adjMatrix)
        & lower.tri(dMatrix)]

    # add groups
    igraph::V(result)$group <- as.factor(c(0, trainGroups))

    result
}

.networksFromDistMatrix <- function(dMatrix, nTrain, N, trainGroups) {
    iTest <- (nTrain + 1):(dim(dMatrix)[1])
    res <- lapply(iTest, function(i) .computeTestNetwork(dMatrix[c(i, 1:nTrain),
        c(i, 1:nTrain)], N, trainGroups))
    res
}

.visitEdges<- function(net, weighted, maxDist) {
    visited <- c()
    toVisit <- rep(FALSE, length(V(net)))
    toVisit[1] <- TRUE
    while (any(toVisit)) {
        neig <- neighbors(net, root)
    }

}


