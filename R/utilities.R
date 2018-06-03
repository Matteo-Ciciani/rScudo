#' @include class.R accessors.R
NULL

# SCUDO FUNCTIONS --------------------------------------------------------------

# .InputCheck ------------------------------------------------------------------

.InputCheck <- function(expressionData, groups, nTop, nBottom, pValue,
                        prepro, featureSel, p.adj) {

    # checks on expressionData -------------------------------------------------

    stopifnot(is.data.frame(expressionData))

    if (!all(sapply(expressionData, is.numeric))) {
        stop("expressionData contains some non-numeric data.")
    }

    if (any(is.na(expressionData))) {
        stop(paste(deparse(substitute(expressionData)),
                   "contains NAs."))
    }

    # checks on groups ---------------------------------------------------------

    stopifnot(is.factor(groups))

    if (any(is.na(groups))) {
        stop(paste(deparse(substitute(groups)),
                   "contains NAs."))
    }

    if (length(groups) != dim(expressionData)[2]) {
        stop(paste("Length of", deparse(substitute(groups)),
                   "os different from number of columns of ",
                   deparse(substitute(expressionData))))
    }

    if (length(groups) == 0) {
        stop("groups has length 0.")
    }

    # checks on nTop and nBottom -----------------------------------------------

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

    if (is.nan(nTop) | is.nan(nBottom)) {
        stop("nTop and nBottom cannot be NaN.")
    }

    if (is.na(nTop) | is.na(nBottom)) {
        stop("nTop and nBottom cannot be NA.")
    }

    if ((nTop %% 1 != 0) | (nBottom %% 1 != 0)) {
        stop("nTop and nBottom must be integers.")
    }

    if ((nTop + nBottom) > dim(expressionData)[1]) {
        stop(paste("top and bottom signatures overlap, expressionData has",
                   "only", dim(expressionData)[1], "rows."))
    }

    # checks on pValue, prepro, featureSel and p.adj ---------------------------

    stopifnot(is.numeric(pValue),
              length(pValue) == 1,
              is.vector(pValue),
              pValue > 0,
              pValue <= 1)

    if (is.nan(pValue)) {
        stop("pValue cannot be NaN")
    }

    if (is.na(pValue)) {
        stop("pValue cannot be NA.")
    }

    stopifnot(is.logical(prepro),
              is.logical(featureSel),
              is.vector(prepro),
              is.vector(featureSel),
              length(prepro) == 1,
              length(featureSel) == 1,
              is.character(p.adj),
              is.vector(p.adj),
              length(p.adj) == 1)

    if (!(p.adj %in% stats::p.adjust.methods)) {
        stop("p.adj should be one of “holm”, “hochberg”, “hommel”, “bonferroni”,
            “BH”, “BY”, “fdr”, “none”. Check stats::p.adjust documentation.")
    }
}


# .FeatureSelection ------------------------------------------------------------

.FeatureSelection <- function(expressionData, pValue, groups,
                              nGroups, featureSel, p.adj) {
    if (nGroups == 2) {
        pVals <- apply(expressionData, 1, function(x) {
            stats::wilcox.test(x[groups == levels(groups)[1]],
                x[groups == levels(groups)[2]], correct = FALSE,
                exact = FALSE)$p.value })
    } else {
        pVals <- apply(expressionData, 1, function(x) {
            stats::kruskal.test(x, groups)$p.value
        })
    }
    pVals <- stats::p.adjust(pVals, method = p.adj)
    expressionData <- expressionData[pVals <= pValue, ]

    expressionData
}

# .Normalization ---------------------------------------------------------------

.Normalization <- function(ExpressionData, groups) {
    virtControl <- rowMeans(vapply(levels(groups), function(x) {
        rowMeans(ExpressionData[groups == x]) },
        rep(0.0, dim(ExpressionData)[1])))
    normExData <- ExpressionData / virtControl
    normExData
}

# .performscudo ----------------------------------------------------------------

.computeES <- function(top, bottom, profile) {
    # top: numeric, indeces of top genes
    # bottom: numeric, indeces of bottom genes
    # profile: numeric, indeces returned by order(espressionData)

    # top signature ------------------------------------------------------------
    # alternative implementation, to test, should be faster
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
    if (any(nonZero)) {
        distances[nonZero] <- distances[nonZero] -
            floor(100 * min(distances[nonZero])) / 100
    }

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
    UpSig <- as.data.frame(sigMatrix, stringsAsFactors = FALSE)[1:nTop, ]
    DwnSig <- as.data.frame(sigMatrix, stringsAsFactors = FALSE)[
        (nTop + 1):nrow(sigMatrix), ]
    rownames(DwnSig) <- 1:nBottom
    if (length(levels(groups)) == 1) {
        consVec <- as.vector(consensusSigMatrix)
        ConsUpSig <- data.frame(consVec[1:nTop], stringsAsFactors = FALSE)
        colnames(ConsUpSig) <- levels(groups)
        ConsDwnSig <- data.frame(consVec[(nTop + 1):nrow(sigMatrix)],
                                 stringsAsFactors = FALSE)
        colnames(ConsDwnSig) <- levels(groups)
    } else {
        ConsUpSig <- as.data.frame(consensusSigMatrix,
            stringsAsFactors = FALSE)[1:nTop, ]
        ConsDwnSig <- as.data.frame(consensusSigMatrix,
            stringsAsFactors = FALSE)[(nTop + 1):nrow(sigMatrix), ]
        rownames(ConsDwnSig) <- 1:nBottom
    }

    ScudoResults(DistMatrix = distances,
        UpSignatures = UpSig,
        DownSignatures = DwnSig,
        Groups = groups,
        ConsensusUpSignatures = ConsUpSig,
        ConsensusDownSignatures = ConsDwnSig,
        SelectedFeatures = rownames(expressionData),
        Params = list(nTop = nTop, nBottom = nBottom, pValue = ..1)
    )
}













