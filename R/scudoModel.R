#' @include scudoClassify.R

#' @export
scudoModel <- function(nTop, nBottom, N, maxDist, weighted, complete, beta,
    distFun = NULL) {

    # TODO: implement inputCheck

    parameters <- data.frame(
        parameter = c("nTop", "nBottom", "N", "maxDist", "weighted", "complete",
            "beta"),
        class = c(rep("numeric", 4), "logical", "logical", "numeric"),
        label = c("nTop", "nBottom", "N", "maxDist", "weighted", "complete",
            "beta"))

    grid <- function(x, y, len = NULL, search = "grid") {
        # possibly improve case complete = c(TRUE, FALSE)
        expand.grid(nTop = nTop, nBottom = nBottom, N = N, maxDist = maxDist,
            weighted = weighted, complete = complete, beta = beta)
    }

    fit <- function(x, y, wts, param, lev, last, weights, classProbs, ...) {
        out <- list(trainData = x, trainGroups = y, pars = param)
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
