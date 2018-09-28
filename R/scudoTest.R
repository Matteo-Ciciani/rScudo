#' @include class.R accessors.R utilities.R
NULL

# scudoTest ----------------------------------------------------------------

#' Performs scudoTest on test Expression Data
#'
#' Performes SCUDO on test Expression Data using feature selected from a
#' previous computed train Scudo Results object.
#'
#' \code{scudoTest} works as a common predict function by testing on a
#' different expressionData object previous feature selection results obtained
#' in a train Scudo Result object. This could be helpful in order to check the
#' effectiveness of previously chosen parameters.
#'
#' @param trainScudoRes  scudoResults object used as training object.
#'
#' @param testExpData data.frame object containing expressionData used as test.
#'
#' @param testGroups factor containing sampleGroups labels for samples in testExpData.
#'
#' @param nTop number of up-regulated features to include in the signatures.
#'
#' @param nBottom number of down-regulated features to include in the
#' signatures.
#'
#' @param norm logical, whether or not to normalize the test expression data.
#'   See Details for a description of the normalization used.
#'
#' @param groupedNorm logical, whether or not to performed grouped
#' normalization. See Details for a description of the normalization used
#'
#' @param distFun the function used to compute the distance between two
#' samples. See Details
#'
#' @return S4 class object \linkS4class{scudoResults}.
#'
#' @export
scudoTest <- function(trainScudoRes, testExpData, testGroups = NULL,
                         nTop = NULL, nBottom = NULL, norm = TRUE,
                         groupedNorm = FALSE, distFun = NULL) {

    # InputCheck ---------------------------------------------------------------

    if (is(trainScudoRes, "scudoResults")) {
        if (is.null(nTop)) nTop <- params(trainScudoRes)$nTop
        if (is.null(nBottom)) nBottom <- params(trainScudoRes)$nBottom
    } else {
        stop("trainScudoRes must be an object of class scudoResults. Current",
             " class is ", class(trainScudoRes))
    }

    testG <- testGroups
    if (is.null(testGroups)) testG <- factor(rep("a", dim(testExpData)[2]))

    # use placeholder for alpha, featureSel, pAdj
    .inputCheck(testExpData, testG, nTop, nBottom, alpha = 0.5,
                norm, groupedNorm, featureSel = FALSE, parametric = FALSE,
                pAdj = "none", distFun = NULL)

    nTest <- length(levels(testGroups))
    nTrain <- length(levels(sampleGroups(trainScudoRes)))

    if (!is.null(testGroups)) {
        if (!isTRUE(all.equal(nTest, nTrain))) {
            message("Train and test have different number of sampleGroups")
        }
    }

    # normalization ------------------------------------------------------------

    testGroups <- testGroups[, drop = TRUE]
    normGroups <- if(groupedNorm) testGroups else NULL

    if (norm) testExpData <- .normalization(testExpData, normGroups)

    # Test Feature Selection ---------------------------------------------------

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

    .performScudo(testExpData, testGroups, nTop, nBottom, distFun, norm,
                  groupedNorm)

}

