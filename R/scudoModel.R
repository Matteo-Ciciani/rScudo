#' @include scudoClassify.R

#' @export
scudoModel <- function(nTop, nBottom, N, maxDist, weighted, complete, beta,
    distFun = NULL) {

    # InputCheck ---------------------------------------------------------------
        
    stopifnot(is.numeric(N),
        is.vector(N),
        length(N) >= 1,
        all(N > 0),
        all(N <= 1.0),
        is.atomic(N)
    )
        
    stopifnot(is.numeric(nTop),
        is.numeric(nBottom),
        length(nTop) >= 1,
        length(nBottom) >= 1,
        is.vector(nTop),
        is.vector(nBottom),
        is.atomic(nTop), 
        is.atomic(nBottom),
        all(is.finite(nTop)),
        all(is.finite(nBottom)),
        all(nTop > 0),
        all(nBottom > 0))
        
    if (any(is.nan(nTop)) || any(is.nan(nBottom))) {
        stop("nTop and nBottom cannot be NaN.")
    }
    
    if (any(is.na(nTop)) || any(is.na(nBottom))) {
        stop("nTop and nBottom cannot be NA.")
    }
    
    if (any(nTop%% 1 != 0) || any(nBottom%% 1 != 0)) {
        stop("nTop and nBottom must be integers.")
    }
        
    # checks on maxDist ------------------------------------------------------
        
    stopifnot(is.numeric(maxDist),
        is.vector(maxDist),
        is.atomic(maxDist),
        all(maxDist > 0),
        length(maxDist) >= 1,
        all(is.finite(maxDist)),
        all(maxDist%% 1 == 0))
        
    # check beta, weighted, complete -------------------------------------------
        
    stopifnot(is.numeric(beta),
        length(beta) >= 1,
        is.vector(beta),
        is.atomic(beta),
        all(beta > 0))
        
    stopifnot(is.logical(weighted),
        is.logical(complete),
        is.atomic(complete),
        is.vector(weighted),
        is.atomic(weighted),
        is.vector(complete),
        length(weighted) >= 1,
        length(complete) >= 1)
    
    # Model construction -------------------------------------------------------

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
