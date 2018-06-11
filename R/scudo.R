#' @include class.R accessors.R utilities.R
NULL

# scudo ------------------------------------------------------------------------

#' Performs SCUDO on Expression Data
#'
#' Performs optional normalization and feature selection, then creates a Scudo
#' Results object.
#'
#' Details...
#'
#' @param expressionData data.frame object that contains expressionData with
#'   different samples as columns.
#'
#' @param groups factor containing groups labels for samples in expressionData.
#'
#' @param nTop number of down-regulated features to include in the signatures.
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
#' @param p.adj p.adj optionally performed on feature selection step. Default
#'   \code{p.adj = "none"}. Look at \code{\link[stats]{p.adjust.methods}} for
#'   the possible adjustments.
#'
#' @return S4 class object \linkS4class{ScudoResults}.
#'
#' @export
scudo <- function(expressionData, groups, nTop, nBottom, pValue = 0.1,
                  prepro = TRUE, featureSel = TRUE, p.adj = "none") {

    .InputCheck(expressionData, groups, nTop, nBottom, pValue,
                prepro, featureSel, p.adj)

    # Normalization ------------------------------------------------------------

    if (prepro) expressionData <- .Normalization(expressionData, groups)

    # Feature Selection --------------------------------------------------------

    groups <- groups[ , drop = TRUE]
    nGroups <- length(levels(groups))

    if (nGroups == 1) {
        warning(paste("Just one group in", deparse(substitute(groups)),
                      ": skipping feature selection"))
        featureSel <- FALSE
    }

    if (featureSel) {
        expressionData <- .FeatureSelection(expressionData,
                                            pValue, groups, nGroups,
                                            featureSel, p.adj)
        if ((nTop + nBottom) > dim(expressionData)[1]) {
            stop("top and bottom signatures overlap, only ",
                 dim(expressionData)[1], " features selected.")
        }
    }

    # Performing Scudo ---------------------------------------------------------

    .performScudo(expressionData, groups, nTop, nBottom, pValue, prepro,
                  featureSel, p.adj)
}

# scudoPredict ----------------------------------------------------------------

#' @export
scudoPredict <- function(trainScudoRes, testExpData, testGroups,
                         nTop, nBottom, prepro = TRUE) {

    # InputCheck --------------------------------------------------------------

    # use placeholder for pValue, featureSel, p.adj
    .InputCheck(testExpData, testGroups, nTop, nBottom, pValue = 0.5,
                prepro, featureSel = FALSE, p.adj = "none")

    # Normalization -----------------------------------------------------------

    testGroups <- testGroups[, drop = TRUE]

    if (prepro) {
        testExpData <- .Normalization(testExpData, testGroups)
    }

    # Test Feature Selection --------------------------------------------------

    nTest <- length(levels(testGroups))
    nTrain <- length(levels(Groups(trainScudoRes)))

    if (nTest != nTrain) {
        warning("Train and Test have different number of groups.")
    }

    present <- SelectedFeatures(trainScudoRes) %in% rownames(testExpData)
    missing <- SelectedFeatures(trainScudoRes)[!present]

    if (length(missing) != 0) {
        warning(paste(length(missing), "features present in trainScudoRes are",
        "absent in testExpData:\n"))
    }

    testExpData <- testExpData[SelectedFeatures(trainScudoRes)[present], ]

    if ((nTop + nBottom) > dim(testExpData)[1]) {
        stop("top and bottom signatures overlap, only",
             dim(testExpData)[1], "features selected.")
    }
    # Performing Scudo --------------------------------------------------------

    .performScudo(testExpData, testGroups, nTop, nBottom, prepro = TRUE)
}

