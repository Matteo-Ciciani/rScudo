#' @include class.R accessors.R utilities.R
NULL

#' @export
scudo <- function(expressionData, groups, nTop, nBottom, pValue,
                  prepro = TRUE, featureSel = TRUE, p.adj = "none") {
    # Input checks -------------------------------------------------------------
    # use warning and stop
    # checks on expressionData

    # checks on groups

    groups <- groups[ , drop = TRUE]
    nGroups <- length(levels(groups))

    if (nGroups == 1) {
        warning(paste("Just one group in", deparse(substitute(groups)),
                      ": skipping fearure selection"))
    }

    # checks on nTop and nBottom

    # checks on pValue, prepro, featureSel and p.adj

    # Normalization ------------------------------------------------------------

    if (prepro) expressionData <- .Normalization(expressionData, groups)

    # Feature Selection --------------------------------------------------------

    if (featureSel && nGroups > 1) {
        if (nGroups == 2) {
            pVals <- apply(expressionData, 1, function(x) {
                stats::wilcox.test(x[groups == levels(groups)[1]],
                    x[groups == levels(groups)[2]])$p.value })
        } else {
            pVals <- apply(expressionData_temp, 1, function(x) {
                stats::kruskal.test(x, groups)$p.value
            })
        }
        pVals <- stats::p.adjust(pVals, method = p.adj)
        expressionData <- expressionData[pVals <= pValue, ]
    }

    # Performing Scudo --------------------------------------------------------

    .performScudo(expressionData, groups, nTop, nBottom, pValue)
}
