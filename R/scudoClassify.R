#' @include utilities.R scudoClassifyUtilities.R
NULL

#' Performes classification using SCUDO
#'
#' placeholder
#'
#' Placeholder
#'
#' @param trainExpData either an \code{\link[Biobase]{ExpressionSet}},
#' a data.frame or a matrix of gene expression data, with a column for
#' each sample and a row for each feature, to be used as a training set
#'
#' @param testExpData either an \code{\link[Biobase]{ExpressionSet}},
#' a data.frame or a matrix of gene expression data, with a column for
#' each sample and a row for each feature, to be used as a testing set
#'
#' @param N a number between 0 and 1, representing the fraction of the
#' signature-to-signature distances that will be used to draw the graph
#'
#' @param nTop number of up-regulated features to include in the signatures
#'
#' @param nBottom number of down-regulated features to include in the
#' signatures
#'
#' @param trainGroups factor containing group labels for each sample in
#' \code{trainExpData}
#'
#' @param maxDist intger. Only nodes with a distance from a testing node less or
#' equal to \code{maxDist} are used to perform the classification.
#'
#' @param weighted logical, whether to consider the distances associated to the
#' edges to compute the scores for the classification. For a description of the
#' classification method, see Details below
#'
#' @param complete logical, whether to consider all the nodes in the training
#' set to perform the classification. If TRUE, the arguments \code{N},
#' \code{maxDist} and \code{weighted} are ignored. For a description of the
#' classification method, see Details below
#'
#' @param beta a coefficient used to down-weight the influence of distant nodes
#' on the classification outcome. For a description of the
#' classification method, see Details below
#'
#' @param alpha p-value cutoff for the optional feature selection step. If
#' feature selection is skipped, alpha is ignored
#'
#' @param norm logical, whether or not to normalize the expression data. See
#' Details for a description of the normalization used
#'
#' @param featureSel logical, whether or not to perform a feature selection.
#' Feature selection is performed using one of four tests: Student's t-test,
#' ANOVA, Wilcoxon-Mann-Withney test, or Kruskal-Wallis test. The test
#' used depends on the number of groups and the \code{parametric} argument
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
#' @return A \code{data.frame} containing classification scores for each sample
#' in \code{testExpData}
#'
#' @seealso \code{\link{scudo}}
#'
#' @author Matteo Ciciani \email{matteo.ciciani@@gmail.com}
#'
#' @examples
#' a <- "Placeholder"
#'
#' @export
scudoClassify <- function(trainExpData, testExpData, N, nTop, nBottom,
    trainGroups, maxDist = 1, weighted = TRUE, complete = FALSE, beta = 1,
    alpha = 0.1, norm = TRUE, featureSel = TRUE, parametric = FALSE,
    pAdj = "none", distFun = NULL) {

    # InputCheck ---------------------------------------------------------------

    .classifyInputCheck(trainExpData, testExpData, N, nTop, nBottom,
        trainGroups, maxDist, weighted, complete, beta,
        alpha, norm, featureSel, parametric, pAdj, distFun)

    # normalization ------------------------------------------------------------

    trainGroups <- trainGroups[, drop = TRUE]

    if (norm) {
        trainExpData <- .normalization(trainExpData, NULL)
        testExpData <- .normalization(testExpData, NULL)
    }

    # Feature Selection --------------------------------------------------------
    # training set

    nGroupsTrain <- length(levels(trainGroups))

    if (nGroupsTrain == 1 && featureSel) {
        warning("Just one group in groups: skipping feature selection.")
        featureSel <- FALSE
    }

    if (featureSel) {
        trainExpData <- .featureSelection(trainExpData, alpha, trainGroups,
            nGroupsTrain, parametric, pAdj)
        if ((nTop + nBottom) > dim(trainExpData)[1]) {
            stop("top and bottom signatures overlap, only ",
                dim(trainExpData)[1], " features selected.")
        }
    }

    # testing set

    present <- rownames(trainExpData) %in% rownames(testExpData)
    missing <- rownames(trainExpData)[!present]

    if (length(missing) != 0) {
        stop(paste(length(missing), "features present in trainExpData are",
            "absent in testExpData:\n"))
    }

    testExpData <- testExpData[rownames(trainExpData)[present], ]

    # Compute distance matrix with train + test --------------------------------

    if (is.null(distFun)) distFun <- .defaultDist
    distMat <- distFun(cbind(trainExpData, testExpData), nTop, nBottom)

    # Compute scores -----------------------------------------------------------

    if (complete) {
        distMat <- distMat[seq_len(dim(trainExpData)[1]),
            seq((dim(trainExpData)[2] + 1),
            (dim(trainExpData)[2] + dim(testExpData)[2]))]

        # get sums for each new sample
        scores <- stats::aggregate(distMat, by = list(trainGroups),
            FUN = sum)
        scores <- scores[, -1] / table(trainGroups)
        scores <- t(apply(scores, 2, function(x) x/sum(x)))
        colnames(scores) <- levels(trainGroups)
    } else {
        scores <- .computeScores(distMat, N, trainGroups, maxDist, weighted,
            beta)
    }

    scores
}
