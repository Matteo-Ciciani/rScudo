#' @include class.R accessors.R utilities.R
NULL

# scudoTest --------------------------------------------------------------------

#' Performs SCUDO analysis on test dataset
#'
#' A function to perform the SCUDO analysis on test data, given an object of
#' class \code{ScudoResults} used as training model.
#'
#' Given an object of class \code{\linkS4class{ScudoResults}} and a set of
#' expression profiles with unknown classification, \code{scudoTest} performs an
#' analysis similar to \code{\link{scudoTrain}}, computing a list of signatures
#' composed of genes over- and under-expressed in each sample, consensus
#' signatures for each group and a distance matrix that quantifies the
#' similarity between the signatures of pairs of samples.
#'
#' \code{scudoTest} differs from \code{scudoTrain} in the feature selection
#' step: only the features present in the \code{ScudoResults} object taken as
#' input are considered for the follwing steps. The computation of fold-changes,
#' the identification of gene signatures and the computation of the distance
#' matrix are performed as described in the Details of \code{\link{scudoTrain}}.
#'
#' If the classification of samples in the testing dataset is provided, it is
#' only used for annotation purposes.
#'
#' @usage scudoTest(trainScudoRes, testExpData, testGroups = NULL, nTop = NULL,
#'     nBottom = NULL, foldChange = TRUE, groupedFoldChange = FALSE,
#'     logTransformed = NULL, distFun = NULL)
#'
#' @param trainScudoRes an object of class \code{ScudoResult} used as
#' training model
#'
#' @param testExpData either an
#' \code{\link[Biobase:ExpressionSet-class]{ExpressionSet}} or a data.frame or
#' a matrix of gene expression data, with a column for each sample and a row
#' for each feature
#'
#' @param testGroups factor containing group labels for each sample in
#' \code{testExpData}
#'
#' @param nTop number of up-regulated features to include in the signatures. If
#' NULL, it defaults to the value present in \code{trainScudoRes}
#'
#' @param nBottom number of down-regulated features to include in the
#' signatures. If NULL, it defaults to the value present in \code{trainScudoRes}
#'
#' @param foldChange logical, whether or not to compute fold-changes from
#' expression data
#'
#' @param groupedFoldChange logical, whether or not to take into account the
#' groups when computing fold-changes. See Details for a description of the
#' computation of fold-changes
#'
#' @param logTransformed logical or NULL. It indicates whether the data is
#' log-transformed. If NULL, an attempt is made to guess if the data is
#' log-transformed
#'
#' @param distFun the function used to compute the distance between two
#' samples. See Details of \code{\link{scudoTrain}} for the specification of
#' this function
#'
#' @return Object of class \code{\linkS4class{ScudoResults}}.
#'
#' @seealso \code{\link{scudoTrain}}, \code{\link{scudoNetwork}},
#' \code{\linkS4class{ScudoResults}}, \code{\link{scudoClassify}}
#'
#' @author Matteo Ciciani \email{matteo.ciciani@@gmail.com}, Thomas Cantore
#' \email{cantorethomas@@gmail.com}
#'
#' @examples
#' # generate dummy train dataset
#' exprDataTrain <- data.frame(a = 11:20, b = 16:25,
#'     c = rev(1:10), d = c(1:2, rev(3:10)))
#' exprDataTest <- data.frame(e = 1:10, f = 11:20,
#'     g = rev(11:20), h = c(1:2, rev(3:10)))
#' rownames(exprDataTrain) <- rownames(exprDataTest) <- letters[11:20]
#' grpsTrain <- as.factor(c("G1", "G1", "G2", "G2"))
#' nTop <- 2
#' nBottom <- 3
#'
#' # run scudo
#' res <- scudoTrain(exprDataTrain, grpsTrain, nTop, nBottom,
#'     foldChange = FALSE, featureSel = FALSE)
#' show(res)
#'
#' # run scudoTest
#' testRes <- scudoTest(res, exprDataTest, foldChange = FALSE)
#' show(testRes)
#'
#' @export
scudoTest <- function(trainScudoRes, testExpData, testGroups = NULL,
    nTop = NULL, nBottom = NULL, foldChange = TRUE,
    groupedFoldChange = FALSE, logTransformed = NULL, distFun = NULL) {

    # InputCheck ---------------------------------------------------------------

    if (is(testExpData, "ExpressionSet")) {
        testExpData <- as.data.frame(Biobase::exprs(testExpData))
    }
    if (is(testExpData, "matrix")) {
        testExpData <- as.data.frame(testExpData)
    }

    if (is(trainScudoRes, "ScudoResults")) {
        if (is.null(nTop)) nTop <- scudoParams(trainScudoRes)$nTop
        if (is.null(nBottom)) nBottom <- scudoParams(trainScudoRes)$nBottom
    } else {
        stop("trainScudoRes must be an object of class ScudoResults. Current",
            " class is ", class(trainScudoRes))
    }

    testG <- testGroups
    if (is.null(testGroups)) testG <- factor(rep("a", dim(testExpData)[2]))

    # use placeholder for alpha, featureSel, pAdj
    .inputCheck(testExpData, testG, nTop, nBottom, alpha = 0.5,
        foldChange, groupedFoldChange, featureSel = FALSE, logTransformed,
        parametric = FALSE, pAdj = "none", distFun = NULL)

    nTest <- length(levels(testGroups))
    nTrain <- length(levels(groupsAnnotation(trainScudoRes)))

    if (!is.null(testGroups)) {
        if (!isTRUE(all.equal(nTest, nTrain))) {
            message("Train and test have different number of groups")
        }
    }

    # computeFC ------------------------------------------------------------

    testGroups <- testGroups[, drop = TRUE]
    foldChangeGroups <- if(groupedFoldChange) testGroups else NULL

    if (foldChange) {
        testExpData <- .computeFC(testExpData, foldChangeGroups, logTransformed)
    }

    # Test Feature Selection ---------------------------------------------------

    present <- selectedFeatures(trainScudoRes) %in% rownames(testExpData)
    missing <- selectedFeatures(trainScudoRes)[!present]

    if (length(missing) != 0) {
        warning(paste(length(missing), "features present in trainScudoRes are",
            "absent in testExpData:\n"))
    }

    testExpData <- testExpData[selectedFeatures(trainScudoRes)[present], ]

    if ((nTop + nBottom) > dim(testExpData)[1]) {
        stop("top and bottom signatures overlap, only",
            dim(testExpData)[1], "features selected.")
    }

    .performScudo(testExpData, testGroups, nTop, nBottom, distFun, foldChange,
        groupedFoldChange)

}

