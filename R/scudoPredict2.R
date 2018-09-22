#' @include class.R accessors.R utilities.R
NULL


# scudoPredict2 ----------------------------------------------------------------

#' @export
scudoPredict2 <- function(trainScudoRes, trainScudoNet, testExpData, nTop,
                          nBottom, testGroups = NULL, norm = TRUE,
                          neighbours = 1, weighted = TRUE, pruned = FALSE) {

    # InputCheck --------------------------------------------------------------

    # use placeholder for pValue, featureSel, pAdj
    .inputCheck(testExpData, testGroups, nTop, nBottom, pValue = 0.5,
                norm, featureSel = FALSE, pAdj = "none")

    # normalization -----------------------------------------------------------

    testGroups <- testGroups[, drop = TRUE]

    if (norm) {
        testExpData <- .normalization(testExpData, testGroups)
    }

    # Test Feature Selection --------------------------------------------------

    nTest <- length(levels(testGroups))
    nTrain <- length(levels(groups(trainScudoRes)))

    if (nTest != nTrain) {
        warning("Train and Test have different number of groups.")
    }

    present <- selectedFeatures(trainScudoRes) %in% rownames(testExpData)
    missing <- selectedFeatures(trainScudoRes)[!present]

    if (length(missing) != 0) {
        warning(paste(length(missing), "features present in trainScudoRes are",
                      "absent in testExpData:\n"))
    }

    testExpData <- testExpData[selectedFeatures(trainScudoRes)[present], ]

    if ((nTop + nBottom) > dim(testExpData)[1]) {
        stop("top and bottom signatures overlap, only",
             dim(testExpData)[1], "features selected.")
    }
    # Performing Scudo on testing set -----------------------------------------

    testScudoResult <- .performScudo(testExpData, testGroups, nTop, nBottom,
                                     norm = TRUE)

    # Use trainScudoNet to perform classification


}

