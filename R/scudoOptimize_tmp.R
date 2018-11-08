#' @include utilities.R scudoClassifyUtilities.R

scudoForCaret <- function(nTopCand, nBottomCand, NCand, ...){
   
    # TODO: implement inputCheck 
    # TODO: pass parameters to functions from ... for classify 
    
    parameters <- data.frame(parameter = c("nTop", "nBottom", "N"), 
                             class = rep("numeric", 3),
                             label = c("Number Top Signatures", 
                                       "Number Bottom Signatures", 
                                       "N"))
    
    grid <- function(x, y, len = NULL, search = "grid") {
        rngSigUp <- nTopCand
        rngSigDown <- nBottoomCand
        rngN <- NCand 
        out <- data.frame(nTop = rngSigUp, nBottom = rngSigDown, N = rngN)
        out                  
    }
    
    fit <- function(x, y, wts, param, lev, last, weights, classProbs, ...) { 
        out <- list(trainData = x, trainGroups = y, nTop = param$nTop)
        out 
    }
    
    predict <- function(modelFit, newdata, reProc = NULL, submodels = NULL){
        rScudo::scudoClassify(
            trainExpData = t(modelFit$trainData),
            testExpData = t(newdata), 
            trainGroups = modelFit$trainGroups,
            N = 0.25, 
            nTop = modelFit@scudoParams$nTop, 
            nBottom = modelFit@scudoParams$nBottom, 
            featureSel = F,
            foldChange = F, 
            ...)$predicted
    }
    
    prob <- function(modelFit, newdata, preProc = NULL, submodels = NULL){
        rScudo::scudoClassify(
            trainExpData = t(modelFit@scudoParams$trainData),
            testExpData = t(newdata), 
            trainGroups = modelFit@scudoParams$trainGroups,
            N = 0.25, 
            nTop = modelFit@scudoParams$nTop, 
            nBottom = modelFit@scudoParams$nBottom, 
            featureSel = F,
            foldChange = F, 
            ...)$scores
    }
    
    rSC <- list(type = "Classification",
                library = "rScudo",
                parameters = parameters, 
                grid = grid, 
                fit = fit, 
                predict = predict, 
                prob = prob)
    rSC
}




 
