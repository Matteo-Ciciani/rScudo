#' @include class.R accessors.R utilities.R
NULL

# scudoTrain -------------------------------------------------------------------

#' Performs SCUDO analysis
#'
#' SCUDO (Signature-based ClUstering for DiagnOstic purposes) is a rank-based
#' method for the analysis of gene expression profiles This function
#' computes gene signatures for each sample and consensus signatures for each
#' group specified. A distance matrix is also computed, that can be used by the
#' function \code{\link{scudoNetwork}} to generate a graph in which each node is
#' a sample and an edge between two nodes quantitatively represents the
#' similarity between their respective signatures.
#'
#' Given a set of expression profiles with known classification,
#' \code{scudoTrain}
#' computes a list of signatures composed of genes over- and under-expressed
#' in each sample. It also compute consensus signatures for each group and
#' uses the signatures to compute a distance matrix that quantifies the
#' similarity between the signatures of pairs of samples.
#'
#' Before computing the signatures, two optional perprocessing steps are
#' performed. In the first step fold-changes are compured from expression
#' values. If the parameter \code{groupedFoldChange} is \code{TRUE}, the
#' fold-changes
#' are computed in two steps: first the mean expression value for each feature
#' in each group is computed. Then, the fold-changes for each feature are
#' computed dividing the expression values by the mean of the group means.
#' If the the parameter
#' \code{groupedFoldChange} is \code{FALSE}, the fold-changes are
#' computed dividing the expression value of each feature by the mean
#' expression value of that feature (regardless of groups). If the expression
#' values are log-transformed, subtraction is used instead of division.
#'
#' The second optional preprocessing step is a feature selection. This step is
#' performed in order to select relevant features.
#' Feature selection is performed using one of four tests: Student's t-test,
#' ANOVA, Wilcoxon-Mann-Withney test, or Kruskal-Wallis test. The test
#' used depends on the number of groups and the parameter \code{parametric}.
#' The parameter \code{pAdj} controls the method used to adjust p-values for
#' multiple hypothesis testing. For a list of adjustment methods see
#' \code{\link[stats]{p.adjust}}. Features with an adjusted p-value less than
#' \code{alpha} are selected.
#'
#' After these two optional steps, the signatures for each sample are computed.
#' Selected features are ranked according to the expression values (or the
#' fold-change, if computed). Than the
#' first \code{nTop} and the last \code{nBottom} features are selected from the
#' ranked list of features. Two \code{data.frame}s are containing the signatures
#' of up-regulated genes and down-regulated genes for each sample are produced
#' and are contained in the returned object.
#'
#' Consensus top and bottom signatures are computed for each group. The
#' avreage rank for each gene is computed for each group. Features are then
#' ranked according to the average rank in each group and the first
#' \code{nTop} and the last \code{nBottom} genes are selected to form the
#' consensus signatures of each group. Two \code{data.frame}s containing the
#' consensus signatures are produced and are contained in the
#' returned object.
#'
#' Gene signatures are used to compute an all-to-all distance matrix.
#' The distance between two samples quantifies the degree of similarty between
#' the signatures of the two samples. The default method used to compute the
#' distance between two samples is based on GSEA. Specifically, the distance
#' between two samples A and B is computed in three steps. First the enrichment
#' score (ES) of the signaure of sample A against the whole expression profile
#' of  sample B, ES(A, B), is compted.  ES(B, A) is also computed. Since a
#' signature is composed of a top and a bottom part, the ES of a signature in a
#' profile is computed as the average of the ES of the top and the bottom
#' signatures. Then, the distance between two samples is computed as the average
#' ES: \deqn{d(A,B)=(ES(A,B)+ES(B,A))/2}
#' Finally, a rounded value of the minimum non-zero distance is subtracted from
#' all values; the purpose of this transformation is to expand the dynamic range
#' and increase the relative differ ence between distance values.
#'
#' The ES employed by default is also known as the Kolmogorov-Smirnov
#' running sum and is analogous to the ES used in the unweighted early
#' version of GSEA. Alternatively, a user specified function can be used to
#' compute the distance matrix, provided using the parameter \code{distFun}.
#' This function should be of the form
#' \code{function(expressionData, nTop, nBottom)}, where \code{expressionData}
#' is a data.frame of expression profiles and \code{nTop} and \code{nBottom}
#' are the sizes of the signatures. This function should
#' return a symmetric square matrix, with identical names on the rows and the
#' columns, corresponding to the names of the samples in \code{expressionData}.
#'
#' The distance matrix is included in the returned object and can be used to
#' generate a graph of samples using \code{\link{scudoNetwork}}.
#'
#' Note that we use the term distance loosely: from a
#' mathematical point of view, our "distance" is actually a semimetric (it does
#' not satisfy the triangle inequality).
#'
#' @usage scudoTrain(expressionData, groups, nTop, nBottom, alpha = 0.1,
#'     foldChange = TRUE, groupedFoldChange = FALSE, featureSel = TRUE,
#'     logTransformed = NULL, parametric = FALSE, pAdj = "none", distFun = NULL)
#
#' @param expressionData either an \code{\link[Biobase]{ExpressionSet}},
#' a data.frame or a matrix of gene expression data, with a column for
#' each sample and a row for each feature
#'
#' @param groups factor containing group labels for each sample in
#' \code{expressionData}
#'
#' @param nTop number of up-regulated features to include in the signatures
#'
#' @param nBottom number of down-regulated features to include in the
#' signatures
#'
#' @param alpha p-value cutoff for the optional feature selection step. If
#' feature selection is skipped, alpha is ignored
#'
#' @param foldChange logical, whether or not to compute fold-changes from
#' expression data
#'
#' @param groupedFoldChange logical, whether or not to take into account the
#' groups when computing fold-changes. See Details for a description of the
#' computation of fold-changes
#'
#' @param featureSel logical, whether or not to perform a feature selection.
#' Feature selection is performed using one of four tests: Student's t-test,
#' ANOVA, Wilcoxon-Mann-Withney test, or Kruskal-Wallis test. The test
#' used depends on the number of groups and the \code{parametric} argument
#'
#' @param logTransformed logical or NULL. It indicates whether the data is
#' log-transformed. If NULL, an attempt is made to guess if the data is
#' log-transformed
#'
#' @param parametric logical, whether to use a parametric or a non-parametric
#' test for the feature selection
#'
#' @param pAdj pAdj method to use to adjust the p-values in the feature
#' selection step. See \code{\link[stats]{p.adjust.methods}} for a list of
#' adjustment methods
#'
#' @param distFun the function used to compute the distance between two
#' samples. See Details for the specification of this function
#'
#' @return Object of class \code{\linkS4class{scudoResults}}.
#'
#' @seealso \code{\link{scudoTest}}, \code{\link{scudoNetwork}},
#' \code{\linkS4class{scudoResults}}
#'
#' @author Matteo Ciciani \email{matteo.ciciani@@gmail.com}, Thomas Cantore
#' \email{cantorethomas@@gmail.com}
#'
#' @examples
#' # generate dummy dataset
#' exprData <- data.frame(a = 11:20, b = 16:25,
#'     c = rev(1:10), d = c(1:2, rev(3:10)))
#' rownames(exprData) <- letters[11:20]
#' grps <- as.factor(c("G1", "G1", "G2", "G2"))
#' nTop <- 2
#' nBottom <- 3
#'
#' # run scudo
#' res <- scudoTrain(exprData, grps, nTop, nBottom, foldChange = FALSE,
#'     featureSel = FALSE)
#' show(res)
#'
#' # examine top signatures and top consensus signatures
#' upSignatures(res)
#' consensusUpSignatures(res)
#'
#' # examine distance matrix
#' distMatrix(res)
#'
#' @export
scudoTrain <- function(expressionData, groups, nTop, nBottom, alpha = 0.1,
    foldChange = TRUE, groupedFoldChange = FALSE, featureSel = TRUE,
    logTransformed = NULL, parametric = FALSE, pAdj = "none", distFun = NULL) {

    if (is(expressionData, "ExpressionSet")) {
        expressionData <- as.data.frame(Biobase::exprs(expressionData))
    }
    if (is(expressionData, "matrix")) {
        expressionData <- as.data.frame(expressionData)
    }

    .inputCheck(expressionData, groups, nTop, nBottom, alpha, foldChange,
        groupedFoldChange, featureSel, logTransformed, parametric,
        pAdj, distFun)

    # computeFC ------------------------------------------------------------

    groups <- groups[, drop = TRUE]
    ngroups <- length(levels(groups))
    foldChangeGroups <- if(groupedFoldChange) groups else NULL

    if (foldChange) {
        expressionData <- .computeFC(expressionData, foldChangeGroups,
            logTransformed)
    }

    # Feature Selection --------------------------------------------------------

    if (ngroups == 1 && featureSel) {
        warning("Just one group in groups: skipping feature selection.")
        featureSel <- FALSE
    }

    if (featureSel) {
        expressionData <- .featureSelection(expressionData,
            alpha, groups, ngroups, parametric, pAdj)
        if ((nTop + nBottom) > dim(expressionData)[1]) {
            stop("top and bottom signatures overlap, only ",
                dim(expressionData)[1], " features selected.")
        }
    }

    # Performing Scudo ---------------------------------------------------------

    .performScudo(expressionData, groups, nTop, nBottom, distFun, alpha,
        foldChange, groupedFoldChange, featureSel, parametric, pAdj)
}

