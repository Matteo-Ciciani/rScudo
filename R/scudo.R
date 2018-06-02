#' @include class.R accessors.R utilities.R
NULL

#' @export
scudo <- function(expressionData, groups, nTop, nBottom, pValue,
                  prepro = TRUE, featureSel = TRUE, p.adj = "none") {
    # Input checks -------------------------------------------------------------
    # use warning and stop
    # checks on expressionData

    stopifnot(is.matrix(expressionData))

    if (any(is.na(expressionData)) & !any(is.nan(expressionData))) {
        stop(paste(deparse(substitute(expressionData)),
                   "contains NA values."))
    }

    if (any(is.nan(expressionData))) {
        stop(paste(deparse(substitute(expressionData)),
                   "contains NaN's values."))
    }

    if (!all(is.numeric(expressionData))) {
        stop(paste(deparse(substitute(expressionData)),
                  "contains non numeric values."))
    }

    if (is.null(rownames(expressionData))) {
        (paste(deparse(substitute(expressionData)),
                   "has no row names."))
    }

    if(is.null(colnames(expressionData))) {
        stop(paste(deparse(substitute(expressionData)),
                   "has no col names."))
    }

    # checks on groups

    stopifnot(is.factor(groups))

    if (length(groups) != dim(expressionData)[2]) {
        stop(paste(deparse(substitute(groups)),
                   "has different length from ",
                   deparse(substitute(expressionData)), "columns."))
    }

    groups <- groups[ , drop = TRUE] #thinking about other checks on groups
    nGroups <- length(levels(groups))

    if (nGroups == 1) {
        warning(paste("Just one group in", deparse(substitute(groups)),
                      ": skipping feature selection"))
    }

    # checks on nTop and nBottom

    stopifnot(is.numeric(nTop), is.numeric(nBottom))

    if (is.nan(nTop) | is.nan(nBottom)) {
        stop("NaN values for nTop and nBottom not allowed.")
    }

    if ((nTop & nBottom) == 0) {
        warning("No selection for nTop or nBottom if = 0")
    }

    if ((nTop & nBottom) < 0) {
        stop("nTop and nBottom must be positive numbers.")
    }

    stopifnot((nTop %% 1 == 0) , (nBottom %% 1 == 0))

    # checks on pValue, prepro, featureSel and p.adj

    stopifnot(is.numeric(pValue))

    if (is.nan(pValue)) {
       stop("pValue = NaN not allowed.")
    }

    if ((pValue < 0) | (pValue > 1))  {
        stop("pValue must be 0 < pVal < 1")
    }

    if (!is.logical(prepro)) {
        stop("prepro is not logical.")
    }

    if (!is.logical(featureSel)) {
        stop("featureSel is not logical.")
    }

    labs <- c("holm", "hochberg", "hommel",
        "bonferroni", "BH", "BY", "fdr", "none")
    if (!(p.adj %in% labs)) {
        stop("p.adj given correction method not available.\n",
             "Check stats::p.adjust documentation for possible options")
    }
    # Normalization ------------------------------------------------------------

    if (prepro) expressionData <- .Normalization(expressionData, groups)

    # Feature Selection --------------------------------------------------------

    if (featureSel && nGroups > 1) {
        if (nGroups == 2) {
            pVals <- apply(expressionData, 1, function(x) {
                stats::wilcox.test(x[groups == levels(groups)[1]],
                    x[groups == levels(groups)[2]], correct = FALSE,
                    exact = FALSE)$p.value })
        } else {
            pVals <- apply(expressionData, 1, function(x) {
                stats::kruskal.test(x, groups)$p.value
            })
        }
        pVals <- stats::p.adjust(pVals, method = p.adj)
        expressionData <- expressionData[pVals <= pValue, ]
    }

    # Checks if new ExprData rows < (nTop + nBottom)

    if ((nTop + nBottom) > dim(expressionData)[1]) {
        stop("nTop and nBottom signatures overlap.")
    }

    # Performing Scudo --------------------------------------------------------

    .performScudo(expressionData, groups, nTop, nBottom, pValue)
}
