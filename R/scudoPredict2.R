#' @include class.R accessors.R utilities.R
NULL

# scudoPredict2 ----------------------------------------------------------------

#' @export
scudoClassify <- function(trainExpData, testExpData, N, nTop, nBottom,
                          trainGroups, testGroups = NULL, pVaue = 0.1,
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
        trainExpData <- .featureSelection(trainExpData, pValue, trainGroups,
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
                                   distFun, pValue, norm, groupedNorm,
                                   featureSel, parametric, pAdj)

    # compute whole distance matrix, then select submatrix with distances from
    # samples in the testing set (columns) from the samples in the training set
    # (rows)

    if (is.null(distFun)) distFun <- .defaultDist

    distMat <- distFun(cbind(trainExpData, testExpData), nTop, nBottom)
    distMat <- distMat[1:dim(trainExpData)[1], (dim(trainExpData)[2] + 1):
                           (dim(trainExpData)[2] + dim(testExpData)[2])]

    # classification: if unweighted count, for each vector, the number of nodes
    # for each group that are connected and pass the N threshold; if weighted:
    # if pruned exclude nodes based on N, if not only compute mean weight and
    # perform test (t or w), issue classification and p-value. Possibly
    # consider different number of neighbours

    if (weighted) {
        if (pruned) {

        } else {
            distSums <- aggregate(distMat, by = list(trainGroups), FUN = sum)

        }
    } else {

    }

    # compute signatures
}

