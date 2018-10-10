#' @include utilities.R
NULL

.classifyInputCheck <- function(trainExpData, testExpData, N, nTop, nBottom,
    trainGroups, neighbors, weighted, complete,
    beta, testGroups, alpha, norm, groupedNorm,
    featureSel, parametric, pAdj, distFun) {

    # checks on expressionData -------------------------------------------------

    stopifnot(is.data.frame(trainExpData) &
        is.data.frame(testExpData))

    if (!all(vapply(trainExpData, is.numeric, logical(1)))) {
        stop("trainExpData contains some non-numeric data.")
    }

    if(!all(vapply(testExpData, is.numeric, logical(1)))) {
        stop("testExpData contains some non-numeric data.")
    }

    if (any(is.na(trainExpData))) {
        stop("rainExpData contains NAs.")
    }

    if (any(is.na(testExpData))) {
        stop("testExpData contains NAs")
    }

    # checks on N, nTop, nBottom -----------------------------------------------

    stopifnot(
        is.numeric(N),
        is.vector(N),
        length(N) == 1,
        N > 0,
        N <= 1.0
    )

    stopifnot(is.numeric(nTop),
        is.numeric(nBottom),
        length(nTop) == 1,
        length(nBottom) == 1,
        is.vector(nTop),
        is.vector(nBottom),
        is.finite(nTop),
        is.finite(nBottom),
        nTop > 0,
        nBottom > 0)

    if (is.nan(nTop) || is.nan(nBottom)) {
        stop("nTop and nBottom cannot be NaN.")
    }

    if (is.na(nTop) || is.na(nBottom)) {
        stop("nTop and nBottom cannot be NA.")
    }

    if ((nTop %% 1 != 0) || (nBottom %% 1 != 0)) {
        stop("nTop and nBottom must be integers.")
    }

    # checks on trainGroups, testGroups ----------------------------------------

    stopifnot(is.factor(trainGroups))

    if (any(is.na(trainGroups))) {
        stop("trainGroups contains NAs.")
    }

    if (length(trainGroups) != dim(trainExpData)[2]) {
        stop(paste("Length of trainGroups is different from number of columns",
            "of trainExpData"))
    }

    if (length(trainGroups) == 0) {
        stop("trainGroups has length 0.")
    }

    if (!is.null(testGroups)) {

        stopifnot(is.factor(testGroups))

        if (any(is.na(testGroups))) {
            stop("testGroups contains NAs")
        }

        if (length(testGroups) != dim(testExpData)[2]) {
            stop(paste("Length of testGroups is different from number of",
                "columns of testExpData"))
        }

        if (length(testGroups) == 0) {
            stop("testGroups has length 0.")
        }
    }

    # checks on neighbors ------------------------------------------------------

    stopifnot(is.numeric(neighbors),
        is.vector(neighbors),
        neighbors > 0,
        length(neighbors) == 1,
        is.finite(neighbors),
        (neighbors %% 1 == 0))

    # checks on alpha, norm, featureSel, groupedNorm, parametric, pAdj ---------

    stopifnot(is.numeric(alpha),
        length(alpha) == 1,
        is.vector(alpha),
        alpha > 0,
        alpha <= 1)

    stopifnot(is.numeric(beta),
        length(beta) == 1,
        is.vector(beta),
        beta > 0)

    stopifnot(is.logical(norm),
        is.logical(featureSel),
        is.logical(groupedNorm),
        is.logical(parametric),
        is.logical(weighted),
        is.logical(complete),
        is.vector(norm),
        is.vector(featureSel),
        is.vector(groupedNorm),
        is.vector(parametric),
        is.vector(weighted),
        is.vector(complete),
        length(norm) == 1,
        length(featureSel) == 1,
        length(groupedNorm) == 1,
        length(parametric) == 1,
        length(weighted) == 1,
        length(complete) == 1,
        is.character(pAdj),
        is.vector(pAdj),
        length(pAdj) == 1)

    if (!(pAdj %in% stats::p.adjust.methods)) {
        stop(paste('pAdj should be one of "holm", "hochberg", "hommel",',
            '"bonferroni", "BH", "BY", "fdr", "none".',
            'Check stats::p.adjust documentation.'))
    }

    # check on distFun ---------------------------------------------------------

    if (!is.null(distFun)){
        stopifnot(is.function(distFun))
        if (length(formals(distFun)) != 3) {
            stop(paste('distFun should take as input three arguments:',
                'expressionData, nTop, nBottom'))
        }
    }
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
    igraph::V(result)$group <- c("0", as.character(trainGroups))

    result
}

.networksFromDistMatrix <- function(dMatrix, N, trainGroups) {
    nTrain <- length(trainGroups)
    iTest <- (nTrain + 1):(dim(dMatrix)[1])
    minimized <- lapply(iTest, function(i) .minimize(dMatrix[c(i,
        seq_len(nTrain)), c(i, seq_len(nTrain))]))
    lapply(minimized, .computeTestNetwork, N, trainGroups)
}

.getScores <- function(net, root, nodes) {
    i <- vapply(names(nodes), function(x) igraph::get.edge.ids(net,
        c(names(root), x), directed = FALSE), numeric(1))
    2 - igraph::get.edge.attribute(net, "distance", i)
}

.visitEdges <- function(net, maxDist, groups, weighted, beta) {
    root <- igraph::V(net)[1]
    bfsRes <- igraph::bfs(net, root, unreachable = FALSE, dist = TRUE)
    toVisit <- rep(FALSE, length(igraph::V(net)))
    toVisit[bfsRes$dist <= maxDist - 1] <- TRUE
    visited <- rep(FALSE, length(igraph::V(net)))
    groupScores <- rep(0, length(groups))
    names(groupScores) <- groups

    while (any(toVisit)) {
        u <- igraph::V(net)[toVisit][1]
        visited[as.integer(u)] <- TRUE
        toVisit[as.integer(u)] <- FALSE
        firstNeighbors <- igraph::neighborhood(net, nodes = u, mindist = 1)[[1]]
        firstNeighbors <- firstNeighbors[!(firstNeighbors %in%
            igraph::V(net)[visited])]
        neighborsGroups <- as.factor(firstNeighbors$group)
        if (weighted) {
            neighborsScores <- .getScores(net, u, firstNeighbors)
            coeff <- beta ^ bfsRes$dist[u]
            neighborsScores <- coeff * neighborsScores
        } else {
            neighborsScores <- rep(1, length(neighborsGroups))
        }
        newScores <- vapply(split(neighborsScores, neighborsGroups), sum,
                            numeric(1))
        groupScores[as.character(levels(neighborsGroups))] <- groupScores[
            as.character(levels(neighborsGroups))] + newScores
    }

    groupScores <- groupScores / sum(groupScores)
    groupScores
}

.computeScores <- function(dMatrix, N, trainGroups, maxDist, weighted, beta) {
    # make test networks and run weighted edge visit on each network
    nets <- .networksFromDistMatrix(dMatrix, N, trainGroups)
    scores <- lapply(nets, .visitEdges, maxDist, levels(trainGroups), weighted,
        beta)
    scores <- t(as.data.frame(scores))
    rownames(scores) <- colnames(dMatrix)[
        (length(trainGroups) + 1):(dim(dMatrix)[2])]
    scores
}


