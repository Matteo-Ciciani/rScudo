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
scudoPredict <- function(trainExpData, testExpData, test, train,
                         nTop, nBottom, pValue,
                         prepro = TRUE, featureSel = TRUE, p.adj = "none") {
    # InputCheck --------------------------------------------------------------

    .InputCheck(trainExpData, train, nTop, nBottom, pValue,
                prepro, featureSel, p.adj)


    ## check on testExpData

    stopifnot(is.data.frame(testExpData))

    if (any(!sapply(testExpData, is.numeric))) {
            stop("testExpData contains some not numeric data.")
    }

    if (any(is.na(testExpData))) {

    stop(paste(deparse(substitute(testExpData)),
                   "contains NA values."))
    }

    ## check on test

    stopifnot(is.factor(test))

    if (any(is.na(test))) {
        stop(paste(deparse(substitute(test)),
                   "contains NA values."))
    }

    if (length(test) != dim(testExpData)[2]) {
        stop(paste(deparse(substitute(test)),
                   "has different length from ",
                   deparse(substitute(testExpData)), "columns."))
    }

    if (length(test) == 0) {
        stop("Groups have length 0.")
    }

    # Normalization -----------------------------------------------------------

    if (prepro) {
        trainExpData <- .Normalization(trainExpData, train)
    }

    # Test Feature Selection --------------------------------------------------

    train <- train[, drop = TRUE]
    nTrain <- length(levels(train))
    test <- test[, drop = TRUE]
    nTest <- length(levels(test))

    if (nTest != nTrain) {
        stop("Train and Test have different number of groups.")
    }

    if (featureSel) {
        trainExpData <- .FeatureSelection(trainExpData, pValue, train, nTrain,
                                          featureSel, p.adj)
        testExpData <- testExpData[rownames(trainExpData), ]
    }

    if ((nTop + nBottom) > dim(testExpData)[1]) {
        stop("nTop and nBottom signatures overlap: testExpData too small.")
    }

    # Performing Scudo --------------------------------------------------------

    .performScudo(testExpData, test, nTop, nBottom, pValue)
}

## Should there be the chance to use the same dataframe for train and test???
## The program online uses as group input vector of indexes: in our case they
## are the labels, hence we already consider that the user has divided
## into training and testing.
