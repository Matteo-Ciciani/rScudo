#' @include class.R accessors.R utilities.R
NULL

scudo <- function(expressionData, groups, nTop, nBottom, pValue) {
    # perform some checks on imput data

    # perform normalization and feature selection

    .performScudo(expressionDataFrame, groups, nTop, nBottom, pValue)
}
