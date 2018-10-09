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

.minimize <- function(distances) {
    nonZero <- distances > sqrt(.Machine$double.eps)
    if (any(nonZero)) {
        minVal <- min(distances[nonZero])
        if (!isTRUE(all.equal(minVal, 2))) {
            distances[nonZero] <- distances[nonZero] - floor(100 * minVal) / 100
        }
    }
    distances
}

.networksFromDistMatrix <- function(dMatrix, N, trainGroups) {
    nTrain <- length(trainGroups)
    iTest <- (nTrain + 1):(dim(dMatrix)[1])
    minimized <- lapply(iTest, function(i) .minimize(dMatrix[c(i, 1:nTrain),
        c(i, 1:nTrain)]))
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
        newScores <- vapply(split(neighborsScores, neighborsGroups), sum, 1.0)
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


