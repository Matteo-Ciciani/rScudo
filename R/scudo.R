#' @include class.R accessors.R utilities.R
NULL

# add some parameters
scudo <- function(expressionData, groups, nTop, nBottom, pValue) {
    # perform some checks on input data

    # perform normalization and feature selection

    .performScudo(expressionDataFrame, groups, nTop, nBottom, pValue)
}
