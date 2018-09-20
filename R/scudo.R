#' @include class.R accessors.R utilities.R
NULL

# scudo ------------------------------------------------------------------------

#' Performs SCUDO on Expression Data
#'
#' Performs optional normalization and feature selection, then creates a Scudo
#' Results object.
#'
#' \code{scudo} performs a normalization based on mean features expression
#' levels in different groups, in order to increase the sensitivity for the
#' subsequent analysis.
#'
#' The feature selection process is helpful for subsequent analysis. It is also
#' possible to compute optionally a pValue adjustment for stricter feature
#' selection.
#'
#' @param expressionData data.frame object that contains expressionData with
#'   different samples as columns.
#'
#' @param groups factor containing groups labels for samples in expressionData.
#'
#' @param nTop number of up-regulated features to include in the signatures.
#'
#' @param nBottom number of down-regulated features to include in the signatures.
#'
#' @param pValue pValue cutoff for optional feature selection. If no feature
#'   selection is performed, pValue is ignored.
#'
#' @param prepro logical, whether or not to normalize the expression data. See
#'   Details for a description of the normalization used.
#'
#' @param featureSel Logical, whether or not to perform feature selection.
#'   Feature selection performed through a Wilcoxon-Mann-Withney test, or with a
#'   Kruskal-Wallis test, depending on the number of groups.
#'
#' @param pAdj pAdj optionally performed on feature selection step. Default
#'   \code{pAdj = "none"}. Look at \code{\link[stats]{p.adjust.methods}} for
#'   the possible adjustments.
#'
#' @return S4 class object \linkS4class{scudoResults}.
#'
#' @export
scudo <- function(expressionData, groups, nTop, nBottom, pValue = 0.1,
                  prepro = TRUE, featureSel = TRUE, pAdj = "none") {

    .inputCheck(expressionData, groups, nTop, nBottom, pValue,
                prepro, featureSel, pAdj)

    # normalization ------------------------------------------------------------

    if (prepro) expressionData <- .normalization(expressionData, groups)

    # Feature Selection --------------------------------------------------------

    groups <- groups[ , drop = TRUE]
    ngroups <- length(levels(groups))

    if (ngroups == 1) {
        warning(paste("Just one group in", deparse(substitute(groups)),
                      ": skipping feature selection"))
        featureSel <- FALSE
    }

    if (featureSel) {
        expressionData <- .featureSelection(expressionData,
                                            pValue, groups, ngroups,
                                            featureSel, pAdj)
        if ((nTop + nBottom) > dim(expressionData)[1]) {
            stop("top and bottom signatures overlap, only ",
                 dim(expressionData)[1], " features selected.")
        }
    }

    # Performing Scudo ---------------------------------------------------------

    .performScudo(expressionData, groups, nTop, nBottom, pValue, prepro,
                  featureSel, pAdj)
}

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
#' @param testgroups factor containing groups labels for samples in testExpData.
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
scudoPredict <- function(trainScudoRes, testExpData, testgroups,
                         nTop, nBottom, prepro = TRUE) {

    # InputCheck --------------------------------------------------------------

    # use placeholder for pValue, featureSel, pAdj
    .inputCheck(testExpData, testgroups, nTop, nBottom, pValue = 0.5,
                prepro, featureSel = FALSE, pAdj = "none")

    # normalization -----------------------------------------------------------

    testgroups <- testgroups[, drop = TRUE]

    if (prepro) {
        testExpData <- .normalization(testExpData, testgroups)
    }

    # Test Feature Selection --------------------------------------------------

    nTest <- length(levels(testgroups))
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

    .performScudo(testExpData, testgroups, nTop, nBottom, prepro = TRUE)
}

