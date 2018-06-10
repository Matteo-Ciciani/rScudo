#' @import methods
#' @include class.R accessors.R
NULL

# Show -------------------------------------------------------------------------

#' @rdname ScudoResults-class
#' @aliases show,ScudoResults-method
#' @usage NULL
setMethod("show", "ScudoResults", function(object) {
    cat("Object of class ScudoResults",  "\n\n")
    cat("Number of samples      : ", paste(dim(DistMatrix(object))[1]),
        "\n")
    cat("UpSignatures length    : ", paste(Params(object)$nTop),
        "\n")
    cat("DownSignatures length  : ", paste(Params(object)$nBottom),
        "\n")

    nGroups <- length(colnames(ConsensusDownSignatures(object)))
    cat("Number of groups       : ", nGroups, "\n")
    gt <- table(Groups(object)[, drop = TRUE])
    invisible(sapply(names(gt), function(x) cat("   ", x, ": ", gt[x],
        "samples\n")))

    if (length(Params(object)) == 3) {

        cat("ScudoPredict           : ", "Performed\n")
        cat("Normalization          : ", paste0(ifelse(Params(object)$prepro,
            "", "not "), "performed"), "\n")

    }else{

        cat("Normalization          : ", paste0(ifelse(Params(object)$prepro,
            "", "not "), "performed"), "\n")
        cat("Feature selection      : ", paste0(
            ifelse(Params(object)$featureSel, "", "not "), "performed"), "\n")

        if (Params(object)$featureSel) {
            if (nGroups == 2) {
                cat("    Test               : ",
                    "Wilcoxon rank sum test", "\n")
            }else{
                cat("    Test               : ",
                    "Kruskal-Wallis rank sum test", "\n")
            }
            cat("    p-value cutoff     : ", paste(Params(object)$pValue),
                "\n")
            cat("    p.adjust method    : ", paste(Params(object)$p.adj),
                "\n")
            cat("    Selected features  : ",
                paste(length(SelectedFeatures(object))))
        }

    }

    invisible(NULL)
})
