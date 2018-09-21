#' @include class.R accessors.R utilities.R
NULL

# scudoPredict ----------------------------------------------------------------

#' Performs scudoPredict on test Expression Data
#'
#' Performes SCUDO on test Expression Data using feature selected from a
#' previous computed train Scudo Results object.
#'
#' \code{scudoPredict} works as a common predict function by testing on a
#' different expressionData object previous feature selection results obtained
#' in a train Scudo Result object. This could be helpful in order to check the
#' effectiveness of previously chosen parameters.
#'
#' @param trainScudoRes  scudoResults object used as training object.
#'
#' @param testExpData data.frame object containing expressionData used as test.
#'
#' @param testGroups factor containing groups labels for samples in testExpData.
#'
#' @param nTop number of up-regulated features to include in the signatures.
#'
#' @param nBottom number of down-regulated features to include in the signatures.
#'
#' @param prepro logical, whether or not to normalize the test expression data.
#'   See Details for a description of the normalization used.
#'
#' @return S4 class object \linkS4class{scudoResults}.
#'
#' @export
scudoPredict <- function(trainScudoRes, testExpData, testGroups,
                         nTop, nBottom, prepro = TRUE) {

    # InputCheck --------------------------------------------------------------

    # use placeholder for pValue, featureSel, pAdj
    .inputCheck(testExpData, testGroups, nTop, nBottom, pValue = 0.5,
                prepro, featureSel = FALSE, pAdj = "none")

    # normalization -----------------------------------------------------------

    testGroups <- testGroups[, drop = TRUE]

    if (prepro) {
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
    # Performing Scudo --------------------------------------------------------

    .performScudo(testExpData, testGroups, nTop, nBottom, prepro = TRUE)
}

