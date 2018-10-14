#' @include class.R accessors.R utilities.R
NULL

# scudoTest --------------------------------------------------------------------

#' Performs SCUDO analysis on test dataset
#'
#' A function to perform the SCUDO analysis on test data, given an object of
#' class \code{scudoResults} used as training model.
#'
#' This function creates an object of class \code{\linkS4class{scudoResults}}
#'
#' Only the features selected in the trainig step are considered for the
#' analysis.
#'
#' Fold-change computation and SCUDO analysis is performed in the same way as
#' explained in \code{\link{scudo}} function details. In this case, groups can
#' also not being specified, as well as nTop and nBottom. If the latter are not
#' specified, same values as the ones in \code{scudoResult} object are used.
#'
#' @usage scudoTest(trainScudoRes, testExpData, testGroups = NULL, nTop = NULL,
#'     nBottom = NULL, foldChange = TRUE, groupedFoldChange = FALSE,
#'     distFun = NULL)
#'
#' @param trainScudoRes an object of class \code{ScudoResult} used as
#' training model
#'
#' @param testExpData either an \code{\link[Biobase]{ExpressionSet}}
#' or a data.frame or a matrix of gene expression data, with a column for
#' each sample and a row for each feature
#'
#' @param testGroups factor containing group labels for each sample in
#' \code{testExpData}
#'
#' @param nTop number of up-regulated features to include in the signatures
#'
#' @param nBottom number of down-regulated features to include in the
#' signatures
#'
#' @param foldChange logical, whether or not to compute fold-changes from
#' expression data
#'
#' @param groupedFoldChange logical, whether or not to take into account the
#' groups when computing fold-changes. See Details for a description of the
#' computation of fold-changes
#'
#' @param distFun the function used to compute the distance between two
#' samples. See See \code{\link{scudo}} function Details for the specification
#' of this function
#'
#' @return Object of class \code{\linkS4class{scudoResults}}.
#'
#' @seealso \code{\link{scudo}}, \code{\link{scudoNetwork}},
#' \code{\linkS4class{scudoResults}}
#'
#' @author Matteo Ciciani \email{matteo.ciciani@@gmail.com}
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
#' res <- scudo(exprDataTrain, grpsTrain, nTop, nBottom, foldChange = FALSE,
#'     featureSel = FALSE)
#' show(res)
#'
#' # run scudoTest
#' testRes <- scudoTest(res, exprDataTest, foldChange = FALSE)
#' show(testRes)
#'
#' @export
scudoTest <- function(trainScudoRes, testExpData, testGroups = NULL,
    nTop = NULL, nBottom = NULL, foldChange = TRUE,
    groupedFoldChange = FALSE, distFun = NULL) {

    # InputCheck ---------------------------------------------------------------

    if (is(testExpData, "ExpressionSet")) {
        testExpData <- as.data.frame(Biobase::exprs(testExpData))
    }
    if (is(testExpData, "matrix")) {
        testExpData <- as.data.frame(testExpData)
    }

    if (is(trainScudoRes, "scudoResults")) {
        if (is.null(nTop)) nTop <- params(trainScudoRes)$nTop
        if (is.null(nBottom)) nBottom <- params(trainScudoRes)$nBottom
    } else {
        stop("trainScudoRes must be an object of class scudoResults. Current",
            " class is ", class(trainScudoRes))
    }

    testG <- testGroups
    if (is.null(testGroups)) testG <- factor(rep("a", dim(testExpData)[2]))

    # use placeholder for alpha, featureSel, pAdj
    .inputCheck(testExpData, testG, nTop, nBottom, alpha = 0.5,
        foldChange, groupedFoldChange, featureSel = FALSE, parametric = FALSE,
        pAdj = "none", distFun = NULL)

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

    if (foldChange) testExpData <- .computeFC(testExpData, foldChangeGroups)

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

