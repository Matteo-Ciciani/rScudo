#' @include class.R accessors.R utilities.R
NULL

# scudo ------------------------------------------------------------------------

#' Performs SCUDO analysis
#'
#' This function performs SCUDO analysis on gene expression data.
#'
#' \code{scudo} performs a normalization based on mean features expression
#' levels in different groups, in order to increase the sensitivity for the
#' subsequent analysis.
#'
#' The feature selection process is helpful for subsequent analysis. It is also
#' possible to compute optionally a pValue adjustment for stricter feature
#' selection.
#'
#' @param expressionData data.frame object that contains expressionData with
#'   different samples as columns
#'
#' @param groups factor containing group labels for each sample in
#' \code{expressionData}
#'
#' @param nTop number of up-regulated features to include in the signatures
#'
#' @param nBottom number of down-regulated features to include in the
#' signatures
#'
#' @param pValue p-value cutoff for the optional feature selection step. If
#' feature selection is skipped, pValue is ignored
#'
#' @param prepro logical, whether or not to normalize the expression data. See
#' Details for a description of the normalization used
#'
#' @param groupedNorm logical, whether or not to performed grouped
#' normalization. See Details for a description of the normalization used
#'
#' @param featureSel logical, whether or not to perform a feature selection.
#' Feature selection is performed using one of four tests: Student's t-test,
#' ANOVA, Wilcoxon-Mann-Withney test, or Kruskal-Wallis test. The test
#' used depends on the number of groups and the \code{parametric} argument
#'
#' @param parametric logical, whether to use a parametric or a non-parametric
#' test for the feature selection
#'
#' @param pAdj pAdj optionally performed on feature selection step. Default
#' \code{pAdj = "none"}. Look at \code{\link[stats]{p.adjust.methods}} for
#' the possible adjustment methods
#'
#' @param distFun the function used to compute the distance between two
#' samples. See Details
#'
#' @return Object of class \linkS4class{scudoResults}.
#'
#' @export
scudo <- function(expressionData, groups, nTop, nBottom, pValue = 0.1,
                  prepro = TRUE, groupedNorm = TRUE, featureSel = TRUE,
                  parametric = FALSE, pAdj = "none", distFun = NULL) {

    .inputCheck(expressionData, groups, nTop, nBottom, pValue,
                prepro, groupedNorm, featureSel, parametric, pAdj, distanceFun)

    # normalization ------------------------------------------------------------

    groups <- groups[, drop = TRUE]
    ngroups <- length(levels(groups))
    normGroups <- ifelse(groupedNorm, groups, NULL)

    if (prepro) expressionData <- .normalization(expressionData, normGroups)

    # Feature Selection --------------------------------------------------------

    if (ngroups == 1) {
        warning(paste("Just one group in", deparse(substitute(groups)),
                      ": skipping feature selection"))
        featureSel <- FALSE
    }

    if (featureSel) {
        expressionData <- .featureSelection(expressionData,
                                            pValue, groups, ngroups,
                                            parametric, pAdj)
        if ((nTop + nBottom) > dim(expressionData)[1]) {
            stop("top and bottom signatures overlap, only ",
                 dim(expressionData)[1], " features selected.")
        }
    }

    # Performing Scudo ---------------------------------------------------------

    .performScudo(expressionData, groups, nTop, nBottom, distFun, pValue,
                  prepro, groupedNorm, featureSel, parametric, pAdj)
}

