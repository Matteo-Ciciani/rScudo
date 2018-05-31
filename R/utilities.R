#' @include class.R accessors.R
NULL

.computeES <- function(top, bottom, profile) {
    # top: numeric, indeces of top genes
    # bottom: numeric, indeces of bottom genes
    # profile: numeric, indeces returned by order(espressionData)

    # top signature ------------------------------------------------------------
    # faster implementation, to test, should be 5x faster
    # membership <- rep(FALSE, length(profile))
    # membership[top] <- TRUE
    # membership <- membership[profile]

    membership <- profile %in% top
    pHits <- cumsum(membership) / length(top)
    pMisses <- cumsum(!membership) / (length(profile) - length(top))
    indexMax <- which.max(abs(pHits - pMisses))
    topES <- pHits[indexMax] - pMisses[indexMax]

    # bottom signature ---------------------------------------------------------
    membership <- profile %in% bottom
    pHits <- cumsum(membership) / length(bottom)
    pMisses <- cumsum(!membership) / (length(profile) - length(bottom))
    indexMax <- which.max(abs(pHits - pMisses))
    bottomES <- pHits[indexMax] - pMisses[indexMax]

    # compute overall ES -------------------------------------------------------
    # returns 1 if top and bottom are respectively at the top and bottom of the
    # profile
    # returns -1 if top and bottom are respectively at the bottom and top of the
    # profile

    ES <- (topES - bottomES) / 2
    ES
}

.computeSignature <- function(indeces, nTop, nBottom) {
    ordNames <- names(indeces)[indeces]
    ordNames[c(1:nTop, (length(ordNames) - nBottom + 1):length(ordNames))]
}

.isZero <- Vectorize(function(x) isTRUE(all.equal(x, 0)))

.performScudo <- function(expressionData, groups, nTop, nBottom, ...) {
    # transform expressionData in index matrix
    indexMatrix <- apply(expressionData, 2, order, decreasing = TRUE)
    rownames(indexMatrix) <- rownames(expressionData)

    # compute signatures
    sigMatrix <- apply(indexMatrix, 2, .computeSignature, nTop, nBottom)

    # compute square non-symmetric matrix, with element[i, j] equal
    # to the ES of signature of sample i in the profile of sample j
    ESmatrix <- outer(colnames(expressionData), colnames(expressionData),
        Vectorize(function(x, y) .computeES(
            indexMatrix[1:nTop, x],
            indexMatrix[(dim(indexMatrix)[1] - nBottom +1):
                (dim(indexMatrix)[1]), x],
            indexMatrix[, y]
            )
        )
    )
    colnames(ESmatrix) <- rownames(ESmatrix) <- colnames(expressionData)

    # compute distance matrix
    distances <- 1 - (ESmatrix + t(ESmatrix)) / 2
    nonZero <- !.isZero(distances)
    distances[nonZero] <- distances[nonZero] -
        floor(100 * min(distances[nonZero])) / 100

    # compute consensus signatures
    rankedExprData <- apply(expressionData, 2, rank)
    groupedRankSums <- stats::aggregate(t(rankedExprData), by = list(groups),
                                        sum)
    rownames(groupedRankSums) <- groupedRankSums[, 1]
    ordGroupedRankSums <- apply(groupedRankSums[, -1], 1, order,
                                decreasing = TRUE)
    rownames(ordGroupedRankSums) <- rownames(expressionData)
    consensusSigMatrix <- apply(ordGroupedRankSums, 2, .computeSignature,
                                nTop, nBottom)

    # create ScudoResults object to return
    ScudoResults(DistMatrix = distances,
        UpSignatures = as.data.frame(sigMatrix[1:nTop, ],
            stringsAsFactors = FALSE),
        DownSignatures = as.data.frame(sigMatrix[(nTop + 1):nrow(sigMatrix), ],
            stringsAsFactors = FALSE),
        Groups = groups,
        ConsensusUpSignatures = as.data.frame(consensusSigMatrix[1:nTop, ],
            stringsAsFactors = FALSE),
        ConsensusDownSignatures = as.data.frame(consensusSigMatrix[(nTop + 1):
            nrow(sigMatrix), ], stringsAsFactors = FALSE),
        SelectedFeatures = rownames(expressionData),
        Params = list(nTop = nTop, nBottom = nBottom, pValue = ..1)
    )
}
