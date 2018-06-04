#' @include class.R accessors.R utilities.R
NULL

# scudo ------------------------------------------------------------------------

#' @export
scudo <- function(expressionData, groups, nTop, nBottom, pValue,
                  prepro = TRUE, featureSel = TRUE, p.adj = "none") {

    .InputCheck(expressionData, groups, nTop, nBottom, pValue,
                prepro, featureSel, p.adj)

    # Normalization ------------------------------------------------------------

    if (prepro) expressionData <- .Normalization(expressionData, groups)

    # Feature Selection --------------------------------------------------------

    groups <- groups[ , drop = TRUE]
    nGroups <- length(levels(groups))

    if (featureSel) {
        expressionData <- .FeatureSelection(expressionData,
                                            pValue, groups, nGroups,
                                            featureSel, p.adj)
    }

    # Checks if nTop, nBottom not high ----------------------------------------

    if ((nTop + nBottom) > dim(expressionData)[1]) {
        stop("nTop and nBottom signatures overlap, expressionData too small.")
    }

    # Performing Scudo --------------------------------------------------------

    .performScudo(expressionData, groups, nTop, nBottom, pValue)

}

# scudoPredict ----------------------------------------------------------------

#' @export
scudoPredict <- function(trainScudoRes, testExpData, testGroups,
                         nTop, nBottom, prepro = TRUE, featureSel = TRUE,
                         p.adj = "none") {

    # InputCheck --------------------------------------------------------------
    pValue <- 1

    .InputCheck(testExpData, testGroups, nTop, nBottom, pValue,
                prepro, featureSel, p.adj)

    # Normalization -----------------------------------------------------------

    if (prepro) {
        testExpData <- .Normalization(testExpData, testGroups)
    }

    # Test Feature Selection --------------------------------------------------

    testGroups <- testGroups[, drop = TRUE]
    nTest <- length(levels(testGroups))
    nTrain <- length(levels(Groups(trainScudoRes)))

    if (nTest != nTrain) {
        stop("Train and Test have different number of groups.")
    }

    temp <- SelectedFeatures(trainScudoRes) %in% rownames(testExpData)
    missing <- SelectedFeatures(trainScudoRes)[!temp]

    if (length(missing) != 0) {
        warning(paste("There are ", length(missing),
        " missing values in testExpData that are present in trainScudoRes:\n",
                list(missing)))
    }

    testExpData <- testExpData[SelectedFeatures(trainScudoRes), ]


    if ((nTop + nBottom) > dim(testExpData)[1]) {
        stop("nTop and nBottom signatures overlap: testExpData too small.")
    }

    # Performing Scudo --------------------------------------------------------

    .performScudo(testExpData, testGroups, nTop, nBottom, pValue)
}

## Should there be the chance to use the same dataframe for train and test???
## The program online uses as group input vector of indexes: in our case they
## are the labels, hence we already consider that the user has divided
## into training and testing
