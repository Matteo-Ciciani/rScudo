#' @include scudoClassify.R
NULL

#' Generate model for \code{caret::train}
#'
#' This function generates a suitable input for the \code{method} argument of
#' the function \code{\link[caret]{train}} from the package \code{caret}, that
#' can be used to perform automatic parameter tuning (e.g. using
#' cross-validation).
#'
#' This function can be used in conjunction with the functions in the package
#' \code{caret} to tune the parameters of \code{\link{scudoClassify}}. The input
#' of this function are vector of parameter values that the tuning procedure
#' should explore. All possible combination of parameter values are explored by
#' default. The user can change this using the \code{search} argument of the
#' \code{\link[caret]{trainControl}} function.
#'
#' The output of this function is a list that represens a classification model
#' using \code{scudoClassify} and that can be used as input for the
#' \code{method} argument of the function \code{\link[caret]{train}}.
#'
#' @usage scudoModel(nTop, nBottom, N, maxDist = 1, weighted = TRUE,
#'     complete = FALSE, beta = 1, distFun = NULL)
#'
#' @param nTop number of up-regulated features to include in the signatures
#'
#' @param nBottom number of down-regulated features to include in the
#' signatures
#'
#' @param N a number between 0 and 1, representing the fraction of the
#' signature-to-signature distances that will be used to draw the graph
#'
#' @param maxDist an integer. Only nodes with a distance from a testing node
#' less or equal to \code{maxDist} are used to perform the classification
#'
#' @param weighted logical, whether to consider the distances associated to the
#' edges to compute the scores for the classification
#'
#' @param complete logical, whether to consider all the nodes in the training
#' set to perform the classification. If TRUE, the arguments \code{N},
#' \code{maxDist}, \code{weighted} and \code{beta} are ignored
#'
#' @param beta a coefficient used to down-weight the influence of distant nodes
#' on the classification outcome
#'
#' @param distFun the function used to compute the distance between two
#' samples. See Details for the specification of this function
#'
#' @return A named list
#'
#' @seealso \code{\link{scudoClassify}}, \code{\link[caret]{train}},
#' \code{\link[caret]{trainControl}}
#'
#' @author Matteo Ciciani \email{matteo.ciciani@@gmail.com}, Thomas Cantore
#' \email{cantorethomas@@gmail.com}
#'
#' @examples
#' # Generate example dataset
#' expData <- data.frame(a = 1:10, b = 2:11, c = 10:1, d = 11:2,
#'     e = c(1:4, 10:5), f = c(7:10, 6:1), g = c(8:4, 1:3, 10, 9),
#'     h = c(6:10, 5:1), i = c(5:1, 6:10))
#' rownames(expData) <- letters[1:10]
#' groups <- factor(c(1,1,1,2,2,2,1,1,1))
#'
#' # Run bootstrap. Notice that the dataset is transposed
#' ctrl <- caret::trainControl(method = "boot", number = 5)
#' model <- scudoModel(nTop = 3:5, nBottom = 3:5, N = 0.5, complete = TRUE)
#' set.seed(1)
#' bootRes <- caret::train(x = t(expData), y = groups, method = model,
#'     trControl = ctrl)
#'
#' @export
scudoModel <- function(nTop, nBottom, N, maxDist = 1, weighted = TRUE,
    complete = FALSE, beta = 1, distFun = NULL) {

    # TODO: implement inputCheck

    parameters <- data.frame(
        parameter = c("nTop", "nBottom", "N", "maxDist", "weighted", "complete",
            "beta"),
        class = c(rep("numeric", 4), "logical", "logical", "numeric"),
        label = c("nTop", "nBottom", "N", "maxDist", "weighted", "complete",
            "beta"))

    grid <- function(x, y, len = NULL, search = "grid") {
        if (isTRUE(all.equal(length(complete), 1)) || isTRUE(all.equal(max(
            length(N), length(maxDist), length(weighted), length(beta)), 1))) {
            expand.grid(nTop = nTop, nBottom = nBottom, N = N,
                maxDist = maxDist, weighted = weighted, complete = complete,
                beta = beta)
        } else {
            gridF <- expand.grid(nTop = nTop, nBottom = nBottom, N = N,
                maxDist = maxDist, weighted = weighted, complete = FALSE,
                beta = beta)
            gridT <- expand.grid(nTop = nTop, nBottom = nBottom, N = N[1],
                maxDist = maxDist[1], weighted = weighted[1], complete = TRUE,
                beta = beta[1])
            message("Ignoring values of N, maxDist, weighted and beta when",
                " complete is TRUE")
            cbind(gridF, gridT)
        }
    }

    fit <- function(x, y, wts, param, lev, last, weights, classProbs, ...) {
        list(trainData = x, trainGroups = y, pars = param)
    }

    predict <- function(modelFit, newdata, reProc = NULL, submodels = NULL){
        scudoClassify(
            trainExpData = t(modelFit$trainData),
            testExpData = t(newdata),
            N = modelFit$pars$N,
            nTop = modelFit$pars$nTop,
            nBottom = modelFit$pars$nBottom,
            trainGroups = modelFit$trainGroups,
            maxDist = modelFit$pars$maxDist,
            weighted = modelFit$pars$weighted,
            complete = modelFit$pars$complete,
            beta = modelFit$pars$beta,
            featureSel = FALSE,
            foldChange = FALSE,
            distFun = distFun)$predicted
    }

    prob <- function(modelFit, newdata, preProc = NULL, submodels = NULL) {
        scudoClassify(
            trainExpData = t(modelFit$trainData),
            testExpData = t(newdata),
            N = modelFit$pars$N,
            nTop = modelFit$pars$nTop,
            nBottom = modelFit$pars$nBottom,
            trainGroups = modelFit$trainGroups,
            maxDist = modelFit$pars$maxDist,
            weighted = modelFit$pars$weighted,
            complete = modelFit$pars$complete,
            beta = modelFit$pars$beta,
            featureSel = FALSE,
            foldChange = FALSE,
            distFun = distFun)$scores
    }

    list(
        type = "Classification",
        library = "rScudo",
        parameters = parameters,
        grid = grid,
        fit = fit,
        predict = predict,
        prob = prob)
}
