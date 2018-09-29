#' @include class.R accessors.R utilities.R
NULL

# scudoTest ----------------------------------------------------------------
#' Performs SCUDO analysis on test data
#'
#' A function to perform SCUDO analysis on test data, given an object of
#' class \code{scudoResults} used as training data. For more informations see
#' Details.
#'
#' Feature selection in testing data is obtained considering just features
#' selected in training data (collected in the \code{scudoResult} object).
#'
#' Data normalization and SCUDO analysis is performed in the same way as
#' explained in \code{\link{scudo}} function details. In this case, groups can
#' also not being specified, as well as nTop and nBottom. If the latter are not
#' specified, same values as the ones in \code{scudoResult} object are used.
#'
#' @param trainScudoRes an object of class \code{ScudoResult} used as
#' training.
#'
#' @param testExpData data.frame of gene expression data, with a column for
#' each sample and a row for each feature, used as testing data.
#'
#' @param testGroups factor containing group labels for each sample in
#' \code{testExpData}
#'
#' @param nTop number of up-regulated features to include in the signatures. See
#' Details.
#'
#' @param nBottom number of down-regulated features to include in the
#' signatures. See Details.
#'
#' @param norm logical, whether or not to normalize the expression data. See
#' Details for a description of the normalization used
#'
#' @param groupedNorm logical, whether or not to performed grouped
#' normalization. See \code{\link{scudo}} function Details for a
#' description of the normalization used
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
#' @author Matteo Ciciani \email{matteo.ciciani@@studenti.unitn.it}
#'
#' @examples
#' # generate dummy train dataset
#' exprData_train <- data.frame(a = 11:20, b = 16:25,
#'             c = rev(1:10), d = c(1:2, rev(3:10)))
#' exprData_test <- data.frame(e = 1:10, f = 11:20,
#'             g = rev(11:20), h = c(1:2, rev(3:10)))
#' rownames(exprData_train) <- rownames(exprData_test) <- letters[11:20]
#' grps_train <- as.factor(c("G1", "G1", "G2", "G2"))
#' nTop <- 2
#' nBottom <- 3
#'
#' # run scudo
#' res <- scudo(exprData_train, grps_train, nTop, nBottom, norm = FALSE,
#'              featureSel = FALSE)
#' show(res)
#'
#' # run scudoTest
#' test_res <- scudoTest(res, exprData_test, norm = FALSE)
#' show(test_res)
#' @export
scudoTest <- function(trainScudoRes, testExpData, testGroups = NULL,
                         nTop = NULL, nBottom = NULL, norm = TRUE,
                         groupedNorm = FALSE, distFun = NULL) {

    # InputCheck ---------------------------------------------------------------

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
                norm, groupedNorm, featureSel = FALSE, parametric = FALSE,
                pAdj = "none", distFun = NULL)

    nTest <- length(levels(testGroups))
    nTrain <- length(levels(groupsAnnotation(trainScudoRes)))

    if (!is.null(testGroups)) {
        if (!isTRUE(all.equal(nTest, nTrain))) {
            message("Train and test have different number of groups")
        }
    }

    # normalization ------------------------------------------------------------

    testGroups <- testGroups[, drop = TRUE]
    normGroups <- if(groupedNorm) testGroups else NULL

    if (norm) testExpData <- .normalization(testExpData, normGroups)

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

    .performScudo(testExpData, testGroups, nTop, nBottom, distFun, norm,
                  groupedNorm)

}

