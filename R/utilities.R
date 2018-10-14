#' @include class.R accessors.R
NULL

# .inputCheck ------------------------------------------------------------------

.inputCheck <- function(expressionData, groups, nTop, nBottom, alpha,
                        norm, groupedNorm, featureSel, parametric, pAdj,
                        distFun) {

    # checks on expressionData -------------------------------------------------

    stopifnot(is.data.frame(expressionData))

    if (!all(vapply(expressionData, is.numeric, logical(1)))) {
        stop("expressionData contains some non-numeric data")
    }

    if (any(is.na(expressionData))) {
        stop(paste(deparse(substitute(expressionData)), "contains NAs."))
    }

    # checks on groups ---------------------------------------------------------

    stopifnot(is.factor(groups))

    if (any(is.na(groups))) {
        stop(paste(deparse(substitute(groups)),
            "contains NAs."))
    }

    if (length(groups) != dim(expressionData)[2]) {
        stop(paste("Length of", deparse(substitute(groups)),
            "is different from number of columns of ",
            deparse(substitute(expressionData))))
    }

    if (length(groups) == 0) {
        stop("groups has length 0")
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

    # check other parameters

    .checkParams(alpha, norm, groupedNorm, featureSel,
        parametric, pAdj, distFun)
}

.checkParams <- function(alpha, norm, groupedNorm, featureSel,
    parametric, pAdj, distFun) {

    # checks on alpha, norm, featureSel, groupedNorm, parametric, pAdj ---------

    stopifnot(is.numeric(alpha),
        length(alpha) == 1,
        is.vector(alpha),
        alpha > 0,
        alpha <= 1)

    if (is.nan(alpha)) {
        stop("alpha cannot be NaN")
    }

    if (is.na(alpha)) {
        stop("alpha cannot be NA.")
    }

    stopifnot(is.logical(norm),
        is.logical(featureSel),
        is.logical(groupedNorm),
        is.logical(parametric),
        is.vector(norm),
        is.vector(featureSel),
        is.vector(groupedNorm),
        is.vector(parametric),
        length(norm) == 1,
        length(featureSel) == 1,
        length(groupedNorm) == 1,
        length(parametric) == 1,
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

# fast tests -------------------------------------------------------------------

.fastWilcoxon <- function (x, y) {
    r <- rank(c(x, y))
    n.x <- as.double(length(x))
    n.y <- as.double(length(y))
    STATISTIC <- c(W = sum(r[seq_along(x)]) - n.x * (n.x + 1) / 2)
    DUPS <- duplicated(r)
    if (any(DUPS)) {
        UNIQ <- r[!DUPS]
        NTIES <- as.list(rep(1, length(UNIQ)))
        names(NTIES) <- UNIQ
        for(val in unique(r[DUPS])) {
            NTIES[[as.character(val)]] <- 1 + sum(r[DUPS] == val)
        }
        NTIES <- unlist(NTIES)
    } else {
        NTIES <- 0
    }
    z <- STATISTIC - n.x * n.y/2
    SIGMA <- sqrt((n.x * n.y/12) * ((n.x + n.y + 1) - sum(NTIES^3 - NTIES)/(
        (n.x + n.y) * (n.x + n.y - 1))))
    z <- z/SIGMA
    PVAL <- 2 * min(stats::pnorm(z), stats::pnorm(z, lower.tail = FALSE))
    PVAL
}

# .featureSelection ------------------------------------------------------------

.featureSelection <- function(expressionData, alpha, groups, ngroups,
    parametric, pAdj) {

    if (parametric) {
        # should we use limma instead?
        if (ngroups == 2) {
            pVals <- apply(expressionData, 1, function(x) {
                stats::t.test(x[groups == levels(groups)[1]],
                x[groups == levels(groups)[2]])$p.value})
        } else {
            pVals <- apply(expressionData, 1, function(x) {
                stats::anova(stats::lm(x ~ groups))["Pr(>F)"][1,1]})
        }
    } else {
        if (ngroups == 2) {
            pVals <- apply(expressionData, 1, function(x) {
                .fastWilcoxon(x[groups == levels(groups)[1]],
                x[groups == levels(groups)[2]])})
        } else {
            pVals <- apply(expressionData, 1, function(x) {
                stats::kruskal.test(x, groups)$p.value})
        }
    }

    pVals <- stats::p.adjust(pVals, method = pAdj)
    expressionData <- expressionData[pVals <= alpha, ]

    expressionData
}

# .normalization ---------------------------------------------------------------

.normalization <- function(ExpressionData, groups) {

    if (is.null(groups)) {
        virtControl <- rowMeans(ExpressionData)
    } else {
        virtControl <- rowMeans(vapply(levels(groups), function(x) {
            rowMeans(ExpressionData[groups == x]) },
            rep(0.0, dim(ExpressionData)[1])))
    }

    qx <- as.numeric(stats::quantile(ExpressionData,
                              c(0., 0.25, 0.5, 0.75, 0.99, 1.0), na.rm=T))
    logC <- (qx[5] > 100) || #checking if not log transfromed
        (qx[6]-qx[1] > 50 && qx[2] > 0) ||
        (qx[2] > 0 && qx[2] < 1 && qx[4] > 1 && qx[4] < 2)

    if (logC) {
        normExData <- ExpressionData / virtControl
    } else {
        normExData <- ExpressionData - virtControl
    }
    normExData
}

# compute distance matrix ------------------------------------------------------

.computeES <- function(top, bottom, profile) {
    # top: numeric, indexes of top genes
    # bottom: numeric, indexes of bottom genes
    # profile: numeric, indexes returned by order(espressionData)

    # faster approach: instead of using cumsum, do the computation only on the
    # indexes in top (or bottom) and on the indexes before them. Complexity
    # should go from linear in the number of feaures to linear in the signature
    # size. There should be enough information in the initial data (top +
    # profile) to figure out on which indexes we need to make computations.

    # top signature
    membership <- rep(FALSE, length(profile))
    membership[top] <- TRUE
    membership <- membership[profile]

    pHits <- cumsum(membership) / length(top)
    pMisses <- cumsum(!membership) / (length(profile) - length(top))
    indexMax <- which.max(abs(pHits - pMisses))
    topES <- pHits[indexMax] - pMisses[indexMax]

    # bottom signature
    membership <- rep(FALSE, length(profile))
    membership[bottom] <- TRUE
    membership <- membership[profile]

    pHits <- cumsum(membership) / length(bottom)
    pMisses <- cumsum(!membership) / (length(profile) - length(bottom))
    indexMax <- which.max(abs(pHits - pMisses))
    bottomES <- pHits[indexMax] - pMisses[indexMax]

    # compute overall ES
    # returns 1 if top and bottom are respectively at the top and bottom of the
    # profile, -1 if viceversa

    ES <- (topES - bottomES) / 2
    ES
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

.defaultDist <- function(expressionData, nTop, nBottom) {

    # compute matrix of indexes
    indexMatrix <- apply(expressionData, 2, order, decreasing = TRUE)

    # compute square non-symmetric matrix, with element[i, j] equal
    # to the ES of signature of sample i in the profile of sample j
    ESmatrix <- outer(colnames(expressionData), colnames(expressionData),
        Vectorize(function(x, y)
            .computeES(
                indexMatrix[seq_len(nTop), x],
                indexMatrix[seq(dim(indexMatrix)[1] - nBottom +1,
                    dim(indexMatrix)[1]), x],
                indexMatrix[, y]
            )
        )
    )
    colnames(ESmatrix) <- rownames(ESmatrix) <- colnames(expressionData)

    # compute distance matrix
    distances <- 1 - (ESmatrix + t(ESmatrix)) / 2
    .minimize(distances)
}

# compute signatures -----------------------------------------------------------

.computeSignature <- function(indeces, nTop, nBottom) {
    ordNames <- names(indeces)[indeces]
    ordNames[c(seq_len(nTop), seq(length(ordNames) - nBottom + 1,
        length(ordNames)))]
}

# perform analysis -------------------------------------------------------------

.performScudo <- function(expressionData, groups, nTop, nBottom,
    distFun = NULL, ...) {

    # transform expressionData in indexes
    indexMatrix <- apply(expressionData, 2, order, decreasing = TRUE)
    rownames(indexMatrix) <- rownames(expressionData)

    # compute signatures
    sigMatrix <- apply(indexMatrix, 2, .computeSignature, nTop, nBottom)

    # compute distance matrix
    distances <- .defaultDist(expressionData, nTop, nBottom)

    # compute consensus signatures
    if (!is.null(groups)) {
        rankedExprData <- as.data.frame(apply(expressionData, 2, rank))
        groupedRankSums <- vapply(levels(groups), function(x) {
            rowSums(rankedExprData[groups == x])},
            rep(0.0, dim(rankedExprData)[1]))
        ordGroupedRankSums <- apply(groupedRankSums, 2, order,
            decreasing = TRUE)
        rownames(ordGroupedRankSums) <- rownames(expressionData)
        consensusSigMatrix <- apply(ordGroupedRankSums, 2, .computeSignature,
            nTop, nBottom)
    }

    # create scudoResults object to return
    UpSig <- as.data.frame(sigMatrix, stringsAsFactors = FALSE)[seq_len(nTop), ]
    DwnSig <- as.data.frame(sigMatrix, stringsAsFactors = FALSE)[
        seq(nTop + 1, nrow(sigMatrix)), ]
    rownames(DwnSig) <- seq_len(nBottom)
    if (is.null(groups)) {
        ConsUpSig <- data.frame()
        ConsDwnSig <- data.frame()
    } else if (length(levels(groups)) == 1) {
        consVec <- as.vector(consensusSigMatrix)
        ConsUpSig <- data.frame(consVec[seq_len(nTop)],
            stringsAsFactors = FALSE)
        colnames(ConsUpSig) <- levels(groups)
        ConsDwnSig <- data.frame(consVec[seq(nTop + 1, nrow(sigMatrix))],
            stringsAsFactors = FALSE)
        colnames(ConsDwnSig) <- levels(groups)
    } else {
        ConsUpSig <- as.data.frame(consensusSigMatrix,
            stringsAsFactors = FALSE)[seq_len(nTop), ]
        ConsDwnSig <- as.data.frame(consensusSigMatrix,
            stringsAsFactors = FALSE)[seq(nTop + 1, nrow(sigMatrix)), ]
        rownames(ConsDwnSig) <- seq_len(nBottom)
    }
    pars <- list(nTop = nTop, nBottom = nBottom)

    if (...length() == 2) {
        pars$norm <- ..1
        pars$groupedNorm <- ..2
    } else {
        pars$alpha <- ..1
        pars$norm <- ..2
        pars$groupedNorm <- ..3
        pars$featureSel <- ..4
        pars$parametric <- ..5
        pars$pAdj <- ..6
    }

    if (is.null(groups)) groups <- factor()

    scudoResults(distMatrix = distances,
        upSignatures = UpSig,
        downSignatures = DwnSig,
        groupsAnnotation = groups,
        consensusUpSignatures = ConsUpSig,
        consensusDownSignatures = ConsDwnSig,
        selectedFeatures = rownames(expressionData),
        params = pars
    )
}

.makeNetwork <- function(dMatrix, N) {
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
    result
}
