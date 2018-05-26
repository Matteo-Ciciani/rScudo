#' @include class.R accessors.R utilities.R
NULL

scudo <- function(expressionData, nUp, nDown, pValue) {
    # perform some checks on imput data

    # perform normalization and feature selection

    # compute signatures and distance matrix -----------------------------------
    #finalExprData needed
    finalExprData <- expressionData # temporary, remove when the previous part is implemented !!!
    # compute signature matrix
    sigMatrix <- apply(finalExprData, 2, function(x) {
        ord_names <- rownames(finalExprData)[order(x, decreasing = TRUE)]
        ord_names[c(1:nUp,
            (nrow(finalExprData) - nDown + 1):nrow(finalExprData))]
        })

    # compute ESmatrix: square non-symmetric matrix, with element[i, j] equal
    # to the ES of signature of sample i in the profile of sample j
    ESmatrix <- outer(colnames(finalExprData), colnames(finalExprData),
        Vectorize(function(x, y) {
            exprProfile <- finalExprData[[y]]
            names(exprProfile) <- rownames(finalExprData)
            .computeES(sigMatrix[1:nUp, x],
                       sigMatrix[(nUp + 1):nrow(sigMatrix), x],
                       exprProfile)
        }))

    Distances <- 1 + (ESmatrix + t(ESmatrix)) / 2

    # compute consensus signatures ---------------------------------------------
}
