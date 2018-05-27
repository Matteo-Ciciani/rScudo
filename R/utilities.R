#' @include class.R accessors.R
NULL

.computeES <- function(top, bottom, profile) {
    # top: character vecotr of gene names
    # bottom: character vecotr of gene names
    # profile: named numeric vector of expression levels

    # sort profile -------------------------------------------------------------
    sortedNames <- rownames(profile)[order(profile, decreasing = TRUE)]

    # top signature ------------------------------------------------------------
    membership <- sortedNames %in% top

    pHits <- cumsum(membership) / length(top)
    pMisses <- cumsum(!membership) / (dim(profile)[1] - length(top))
    indexMax <- which.max(abs(pHits - pMisses))
    topES <- pHits[indexMax] - pMisses[indexMax]

    # bottom signature ---------------------------------------------------------
    membership <- sortedNames %in% bottom
    pHits <- cumsum(membership) / length(bottom)
    pMisses <- cumsum(!membership) / (dim(profile)[1] - length(bottom))
    indexMax <- which.max(abs(pHits - pMisses))
    bottomES <- pHits[indexMax] - pMisses[indexMax]

    # compute overall ES -------------------------------------------------------
    # returns -1 if top and bottom are respectively at the top and bottom of the
    # profile
    # returns 1 if top and bottom are respectively at the bottom and top of the
    # profile

    ES <- (topES - bottomES) / 2
    ES
}

.computeSignature <- function(profile, nTop, nBottom) {
    ordNames <- names(sort(profile, decreasing = TRUE))
    ordNames[c(1:nTop, (length(ordNames) - nBottom + 1):length(ordNames))]
}

.performScudo <- function(expressionData, groups, nTop, nBottom, pValue) {

    sigMatrix <- apply(expressionData, 2, .computeSignature, nTop, nBottom)

    # square non-symmetric matrix, with element[i, j] equal
    # to the ES of signature of sample i in the profile of sample j
    ESmatrix <- outer(colnames(expressionData), colnames(expressionData),
                    Vectorize(function(x, y) {
                        .computeES(sigMatrix[1:nTop, x],
                                   sigMatrix[(nTop + 1):nrow(sigMatrix), x],
                                   expressionData[y])
                    }))
    colnames(ESmatrix) <- rownames(ESmatrix) <- colnames(expressionData)

    distances <- 1 - (ESmatrix + t(ESmatrix)) / 2

    rankedExprData <- apply(expressionData, 2, rank)
    groupedRankSums <- aggregate(t(rankedExprData), by = list(groups), sum)
    rownames(groupedRankSums) <- GroupedRankSums[, 1]
    consensusSigMatrix <- apply(groupedRankSums[, -1], 1, .computeSignature,
                                nTop, nBottom)

    ScudoResults(DistMatrix = distances,
                 UpSignatures = as.data.frame(sigMatrix[1:nTop, ]),
                 DownSignatures =
                     as.data.frame(sigMatrix[(nTop + 1):nrow(sigMatrix), ]),
                 Groups = groups,
                 ConsensusUpSignatures =
                     as.data.frame(consensusSigMatrix[1:nTop, ]),
                 ConsensusDownSignatures =
                     as.data.frame(consensusSigMatrix)[(nTop + 1):
                                                       nrow(sigMatrix), ]
                 Features = rownames(expressionData)
                 Params = list(nTop = nTop, nBottom = nBottom, pValue = pValue)
                 )
}
