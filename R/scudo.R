#' @include class.R accessors.R utilities.R
NULL

scudo <- function(expressionData, groups, nTop, nBottom, pValue,
                  prepro = TRUE, featureSel = TRUE, p.adj = "none")
{
    # Input checks ------------------------------------------------------------
    # Possible implementation considering just 2 on n groups for feature sel?


    groups <- groups[ , drop = TRUE]
    nGroups <- length(levels(groups))

    if (prepro | featureSel) {
        expressionData_temp <- expressionData
        colnames(expressionData_temp) <- groups
    }

    # Normalization -----------------------------------------------------------

    if (prepro) {
        expressionData_temp <- .Normalization(expressionData_temp, groups)
    }

    # Feature Selection -------------------------------------------------------

    if (featureSel) {

        if (nGroups == 1) {
            warning("You have just one group selected:
                    feature selection not performed!")
        }
        if (nGroups == 2) {
            pVals <- apply(expressionData_temp, 1, function(x) {
                stats::wilcox.test(x[names(x) == levels(groups)[1]],
                    x[names(x) == levels(groups)[2]])$p.value
            })

            pVals <- p.adjust(pVals, p.adj)
            expressionData_temp <- expressionData_temp[pVals <= pValue, ]
        }

        if (nGroups > 2) {
            pVals <- apply(expressionData_temp, 1, function(x) {
                stats::kruskal.test(x, groups)$p.value
            })

            pVals <- p.adjust(pVals, p.adj)
            expressionData_temp <- expressionData_temp[pVals <= pValue, ]
        }
    }

    colnames(expressionData_temp) <- colnames(expressionData)

    # Performing Scudo --------------------------------------------------------

    .performScudo(expressionDataFrame, groups, nTop, nBottom, pValue)
}
