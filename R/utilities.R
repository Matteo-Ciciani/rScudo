#' @include class.R accessors.R
NULL

# SCUDO FUNCTIONS -------------------------------------------------------------

# .InputCheck ------------------------------------------------------------------

.InputCheck <- function(expressionData, groups, nTop, nBottom, pValue,
                        prepro, featureSel, p.adj) {

    # checks on expressionData ------------------------------------------------

    stopifnot(is.data.frame(expressionData))

    if (any(!sapply(expressionData, is.numeric))) {
        stop("expressionData contains some not numeric data.")
    }

    if (any(is.na(expressionData))) {
        stop(paste(deparse(substitute(expressionData)),
                   "contains NA values."))
    }

    # checks on groups --------------------------------------------------------

    stopifnot(is.factor(groups))

    if (any(is.na(groups))) {
        stop(paste(deparse(substitute(groups)),
                   "contains NA values."))
    }

    if (length(groups) != dim(expressionData)[2]) {
        stop(paste(deparse(substitute(groups)),
                   "has different length from ",
                   deparse(substitute(expressionData)), "columns."))
    }

    if (length(groups) == 0) {
        stop("Groups have length 0.")
    }
    # checks on nTop and nBottom ----------------------------------------------

    stopifnot(is.numeric(nTop), is.numeric(nBottom),
              (length(nTop) == 1), (length(nBottom) == 1),
              (class(nTop) == "numeric"), (class(nBottom) == "numeric"))

    if (is.na(nTop) | is.na(nBottom)) {
        stop("NA values for nTop and nBottom not allowed.")
    }

    if (is.nan(nTop) | is.nan(nBottom)) {
        stop("NaN values for nTop and nBottom not allowed.")
    }

    if ((nTop <= 0) | (nBottom <= 0)) {
        stop("nTop and nBottom must be positive integer numbers.")
    }

    stopifnot((nTop %% 1 == 0) , (nBottom %% 1 == 0))

    # checks on pValue, prepro, featureSel and p.adj --------------------------

    stopifnot(is.numeric(pValue),
              (length(pValue) == 1),
              (class(pValue) == "numeric"))

    if (is.na(pValue)) {
        stop("pValue = NA not allowed.")
    }

    if (is.nan(pValue)) {
        stop("pValue = NaN not allowed.")
    }

    if (pValue == 0) {
        stop("pValue = 0 given.")
    }

    if (length(pValue) == 0) {
        stop("pValue given is a set of numeric of length 0.")
    }

    if ((pValue < 0) | (pValue > 1))  {
        stop("pValue must be 0 < pVal < 1.")
    }

    stopifnot(is.logical(prepro), is.logical(featureSel),
              class(prepro) == "logical", class(featureSel) == "logical",
              length(prepro) == 1, length(featureSel) == 1)

    if (length(prepro) == 0 | length(featureSel) == 0) {
        stop("Set of logical of length 0 given for prepro or featureSel.")
    }


    if (length(p.adj) == 0) {
        stop("Set of characters for p.adj has length 0.")
    }

    stopifnot(length(p.adj) == 1)

    labs <- c("holm", "hochberg", "hommel",
              "bonferroni", "BH", "BY", "fdr", "none")
    if (!(p.adj %in% labs)) {
        stop("p.adj given correction method not available.\n",
             "Check stats::p.adjust documentation for possible options")
    }
}
# .FeatureSelection ------------------------------------------------------------

.FeatureSelection <- function(expressionData, pValue, groups,
                              nGroups, featureSel, p.adj) {

    if (nGroups == 1) {
        warning(paste("Just one group in", deparse(substitute(groups)),
                      ": skipping feature selection"))
    }

    if (featureSel && nGroups > 1) {
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
    }
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

# .performscudo ---------------------------------------------------------------

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













