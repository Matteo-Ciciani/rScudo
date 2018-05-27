#' @include class.R accessors.R utilities.R
NULL

scudo <- function(expressionData, nUp, nDown, pValue) {
    # perform some checks on imput data

    # perform normalization and feature selection


    # temporary, remove when the previous part is implemented !!!
    finalExprData <- expressionData
    # compute signatures and distance matrix -----------------------------------

    # compute signature matrix
    sigMatrix <- apply(finalExprData, 2, .computeSignature, nUp, nDown)




    # compute consensus signatures ---------------------------------------------
}
