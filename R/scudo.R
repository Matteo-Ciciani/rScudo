#' @include class.R accessors.R utilities.R
NULL

#' @export
scudo <- function(expressionData, groups, nTop, nBottom, pValue,
                  prepro = TRUE, featureSel = TRUE, p.adj = "none") {
    # Input checks -------------------------------------------------------------
    # use warning and stop
    # checks on expressionData

    stopifnot(is.data.frame(expressionData), sapply(expressionData, is.numeric))

    if (any(is.na(expressionData))) {
        stop(paste(deparse(substitute(expressionData)),
                   "contains NA values."))
    }

    # checks on groups

    stopifnot(is.factor(groups))

    if (any(is.na(groups))) {
        stop(paste(deparse(substitute(groups)),
                   "contains NA values."))
    }

    if (length(groups) != dim(expressionData)[2]) {
        stop(paste(deparse(substitute(groups)),
                   "has different length from ",
                   deparse(substitute(expressionData)), "columns."))
    }

    groups <- groups[ , drop = TRUE]
    nGroups <- length(levels(groups))

    if (nGroups == 1) {
        warning(paste("Just one group in", deparse(substitute(groups)),
                      ": skipping feature selection"))
    }

    # checks on nTop and nBottom

    stopifnot(is.numeric(nTop), is.numeric(nBottom))

    if (is.na(nTop) | is.na(nBottom)) {
        stop("NA values for nTop and nBottom not allowed.")
    }

    if (is.nan(nTop) | is.nan(nBottom)) {
        stop("NaN values for nTop and nBottom not allowed.")
    }

    if ((nTop <= 0) | (nBottom <= 0)) {
        stop("nTop and nBottom must be positive integer numbers.")
    }

    stopifnot((nTop %% 1 == 0) , (nBottom %% 1 == 0)) #check if they are integers with is.integer -> is.integer(1) = FALSE -> I've used this way to check if they're integer numbers

    # checks on pValue, prepro, featureSel and p.adj

    stopifnot(is.numeric(pValue))

    if (is.na(pValue)) {
        stop("pValue = NA not allowed.")
    }

    if (is.nan(pValue)) {
       stop("pValue = NaN not allowed.")
    }

    if (pValue == 0) {
        stop("pValue = 0 given.")
    }

    if (length(pValue) == 0) {
        stop("pValue given is a set of numeric of length 0.")
    }

    if ((pValue < 0) | (pValue > 1))  {
        stop("pValue must be 0 < pVal < 1.")
    }

    stopifnot(is.logical(prepro), is.logical(featureSel))

    if (length(prepro) == 0 | length(featureSel) == 0) {
        stop("Set of logical of length 0 given for prepro or featureSel.")
    }


    if (length(p.adj) == 0) {
        stop("Set of characters for p.adj has length 0.")
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
