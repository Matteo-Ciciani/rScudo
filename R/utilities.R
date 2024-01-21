#' @include class.R accessors.R
NULL

# .inputConverter --------------------------------------------------------------

.inputConverter <- function(expressionData) {
    if (is(expressionData, "ExpressionSet")) {
        expressionData <- as.data.frame(Biobase::exprs(expressionData))
    }
    if (is(expressionData, "matrix")) {
        expressionData <- as.data.frame(expressionData)
    }
    if (is(expressionData, "SummarizedExperiment")) {
        if (length(SummarizedExperiment::assays(expressionData)) > 1) {
            warning(paste("Input data contains multiple assays datasets,",
                "just the first one will be considered."))
        }
        expressionData <- as.data.frame(
            SummarizedExperiment::assays(expressionData)[[1]])
    }

    if (!all(vapply(expressionData, is.numeric, logical(1)))) {
        stop("Input data contains some non-numeric data")
    }

    if (any(is.na(expressionData))) {
        stop("Input data contains NAs")
    }

    expressionData
}

# .inputCheck helpers ----------------------------------------------------------

.isSingleLogical <- function(x) {
    is.logical(x) && length(x) == 1L && !is.na(x) && is.vector(x)
}

.isSinglePositiveNumber <- function(x) {
    S4Vectors::isSingleNumber(x) && is.finite(x) && is.vector(x) && x > 0
}

.isSinglePositiveInteger <- function(x) {
    .isSinglePositiveNumber(x) && isTRUE(all.equal(x %% 1, 0))
}

.isValidDistFun <- function(x) {
    is.null(x) || (is.function(x) && length(formals(x)) == 3)
}

.isValidInputVector <- function(x, f) {
    f <- Vectorize(f)
    is.vector(x) && length(x) >= 1 && all(f(x))
}

# input checks -----------------------------------------------------------------

.inputCheck <- function(expressionData, groups, nTop, nBottom, alpha,
    foldChange, groupedFoldChange, featureSel, logTransformed, parametric, pAdj,
    distFun) {

    # check expressionData names

    expColnames <- colnames(expressionData)
    if (length(expColnames) != length(unique(expColnames))) {
        stop("Sample names must be unique")
    }

    # checks on groups

    stopifnot(is.factor(groups))

    if (any(is.na(groups))) stop("groups contain NAs.")

    if (length(groups) != dim(expressionData)[2]) {
        stop(paste("Length of groups is different from number of columns of",
            "expressionData"))
    }

    # checks on nTop and nBottom

    stopifnot(.isSinglePositiveInteger(nTop),
        .isSinglePositiveInteger(nBottom))

    if ((nTop + nBottom) > dim(expressionData)[1]) {
        stop(paste("top and bottom signatures overlap, expressionData has",
            "only", dim(expressionData)[1], "rows."))
    }

    # check other parameters

    .checkParams(alpha, foldChange, groupedFoldChange, featureSel,
        logTransformed, parametric, pAdj, distFun)
}

.checkParams <- function(alpha, foldChange, groupedFoldChange, featureSel,
    logTransformed, parametric, pAdj, distFun) {

    stopifnot(.isSinglePositiveNumber(alpha),
        alpha <= 1)

    if (!is.null(logTransformed)) {
        stopifnot(.isSingleLogical(logTransformed))
    }

    stopifnot(.isSingleLogical(foldChange),
        .isSingleLogical(featureSel),
        .isSingleLogical(groupedFoldChange),
        .isSingleLogical(parametric),
        S4Vectors::isSingleString(pAdj))

    if (!(pAdj %in% stats::p.adjust.methods)) {
        stop(paste('pAdj should be one of "holm", "hochberg", "hommel",',
            '"bonferroni", "BH", "BY", "fdr", "none".',
            'Check stats::p.adjust documentation.'))
    }

    # check on distFun

    if (!.isValidDistFun(distFun)){
        stop(paste('distFun should take as input three arguments:',
            'expressionData, nTop, nBottom'))
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

# .computeFC -------------------------------------------------------------------

.computeFC <- function(ExpressionData, groups, logTransformed) {

    if (is.null(groups)) {
        virtControl <- rowMeans(ExpressionData)
    } else {
        virtControl <- rowMeans(vapply(levels(groups), function(x) {
            rowMeans(ExpressionData[groups == x]) },
            rep(0.0, dim(ExpressionData)[1])))
    }

    if (is.null(logTransformed)) {
        qx <- as.numeric(stats::quantile(ExpressionData,
            c(0., 0.25, 0.5, 0.75, 0.99, 1.0), na.rm = TRUE))
        logTransformed <- !((qx[5] > 100) || (qx[6]-qx[1] > 50 && qx[2] > 0) ||
            (qx[2] > 0 && qx[2] < 1 && qx[4] > 1 && qx[4] < 2))
    }


    if (logTransformed) {
        foldChangeExData <- ExpressionData - virtControl
    } else {
        foldChangeExData <- ExpressionData / virtControl
    }
    foldChangeExData
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

# performScudo -----------------------------------------------------------------

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

    # create ScudoResults object to return
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
        pars$foldChange <- ..1
        pars$groupedFoldChange <- ..2
    } else {
        pars$alpha <- ..1
        pars$foldChange <- ..2
        pars$groupedFoldChange <- ..3
        pars$featureSel <- ..4
        pars$parametric <- ..5
        pars$pAdj <- ..6
    }

    if (is.null(groups)) groups <- factor()

    ScudoResults(distMatrix = distances,
        upSignatures = UpSig,
        downSignatures = DwnSig,
        groupsAnnotation = groups,
        consensusUpSignatures = ConsUpSig,
        consensusDownSignatures = ConsDwnSig,
        selectedFeatures = rownames(expressionData),
        scudoParams = pars
    )
}

# scudoNetwork utilities -------------------------------------------------------

.makeNetwork <- function(dMatrix, N) {
    # compute adjacency matrix
    adjMatrix <- matrix(0, nrow = dim(dMatrix)[1], ncol = dim(dMatrix)[1])
    NQuantile <- stats::quantile(dMatrix[dMatrix > sqrt(.Machine$double.eps)],
        probs = N)
    adjMatrix[dMatrix <= NQuantile] <- 1
    colnames(adjMatrix) <- colnames(dMatrix)

    # generate graph using graph_from_adjacency_matrix
    result <- igraph::graph_from_adjacency_matrix(adjMatrix,
        mode = "max", diag = FALSE)

    # add distances
    igraph::E(result)$distance <- dMatrix[as.logical(adjMatrix)
        & lower.tri(dMatrix)]
    result
}

.addColors <-function(result, object, colors) {
    if (length(groupsAnnotation(object)) == 0) {
        igraph::V(result)$color <- rep("#FFFFFF", dim(distMatrix(object))[1])
    } else {
        igraph::V(result)$group <- as.character(groupsAnnotation(object))

        if (length(colors) == 0) {
            pal <- grDevices::rainbow(length(levels(groupsAnnotation(object))))
            pal <- stringr::str_extract(pal, "^#[0-9a-fA-F]{6}")
            igraph::V(result)$color <- pal[as.integer(groupsAnnotation(object))]
        } else {
            igraph::V(result)$color <- stringr::str_extract(colors,
                "^#[0-9a-fA-F]{6}")
        }
    }

    result
}

# scudoModel utilities ---------------------------------------------------------

.modelInputCheck <- function(nTop, nBottom, N, maxDist, weighted, complete,
    beta, distFun) {

    # check N, nTop, nBottom, maxDist, beta, weighted and complete

    validInput <- all(unlist(BiocGenerics::Map(.isValidInputVector,
        list(N, nTop, nBottom, maxDist, weighted, complete, beta),
        c(.isSinglePositiveNumber, .isSinglePositiveInteger,
            .isSinglePositiveInteger, .isSinglePositiveInteger,
            .isSingleLogical, .isSingleLogical, .isSinglePositiveNumber))))

    if(!validInput && all(N <= 1.0)) stop("Invalid values in input vectors")

    # check distFun

    if (!.isValidDistFun(distFun)){
        stop(paste('distFun should take as input three arguments:',
            'expressionData, nTop, nBottom'))
    }
}
