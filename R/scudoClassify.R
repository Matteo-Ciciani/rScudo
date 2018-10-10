#' @include utilities.R scudoClassifyUtilities.R
NULL

#' Performes classification using SCUDO
#'
#' Placeholder
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
    trainGroups, neighbors = 1, weighted = TRUE,
    complete = FALSE, beta = 1, testGroups = NULL,
    alpha = 0.1, norm = TRUE, groupedNorm = FALSE,
    featureSel = TRUE, parametric = FALSE, pAdj = "none",
    distFun = NULL) {

    # InputCheck ---------------------------------------------------------------

    .classifyInputCheck(trainExpData, testExpData, N, nTop, nBottom,
        trainGroups, neighbors, weighted, complete, beta,
        testGroups, alpha, norm, groupedNorm, featureSel,
        parametric, pAdj, distFun)

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

    if (nGroupsTrain == 1 && featureSel) {
        warning("Just one group in groups: skipping feature selection.")
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

    # Compute distance matrix with train + test --------------------------------

    if (is.null(distFun)) distFun <- .defaultDist
    distMat <- distFun(cbind(trainExpData, testExpData), nTop, nBottom)

    # Compute scores -----------------------------------------------------------

    if (complete) {
        distMat <- distMat[seq_len(dim(trainExpData)[1]),
            seq((dim(trainExpData)[2] + 1),
            (dim(trainExpData)[2] + dim(testExpData)[2]))]

        # get sums for each new sample
        scores <- stats::aggregate(distMat, by = list(trainGroups),
            FUN = sum)
        scores <- scores[, -1] / table(trainGroups)
        scores <- t(apply(scores, 2, function(x) x/sum(x)))
        colnames(scores) <- levels(trainGroups)
    } else {
        scores <- .computeScores(distMat, N, trainGroups, neighbors, weighted,
            beta)
    }

    scores
}

