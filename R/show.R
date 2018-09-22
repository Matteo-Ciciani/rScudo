#' @import methods
#' @include class.R accessors.R
NULL

# Show -------------------------------------------------------------------------

#' @rdname scudoResults-class
#' @aliases show,scudoResults-method
#' @usage NULL
setMethod("show", "scudoResults", function(object) {
    cat("Object of class scudoResults\n")
    cat("Result of", ifelse(length(params(object)) == 3,
        "scudoPredict", "scudo"), "\n\n")
    cat("Number of samples      : ", paste(dim(distMatrix(object))[1]),
        "\n")

    ngroups <- length(colnames(consensusDownSignatures(object)))
    cat("Number of groups       : ", ngroups, "\n")
    gt <- table(groups(object)[, drop = TRUE])
    invisible(sapply(names(gt), function(x) cat("   ", x, ": ", gt[x],
        "samples\n")))

    cat("upSignatures length    : ", paste(params(object)$nTop),
        "\n")
    cat("downSignatures length  : ", paste(params(object)$nBottom),
        "\n")
    cat("Normalization          : ", paste0(ifelse(params(object)$norm,
        "", "not "), "performed"), "\n")

     if (length(params(object)) != 3) {
        cat("Feature selection      : ", paste0(
            ifelse(params(object)$featureSel, "", "not "), "performed"), "\n")

        if (params(object)$featureSel) {
            if (ngroups == 2) {
                cat("    Test               : ",
                    "Wilcoxon rank sum test", "\n")
            }else{
                cat("    Test               : ",
                    "Kruskal-Wallis rank sum test", "\n")
            }
            cat("    p-value cutoff     : ", paste(params(object)$pValue),
                "\n")
            cat("    p.adjust method    : ", paste(params(object)$p.adj),
                "\n")
            cat("    Selected features  : ",
                paste(length(selectedFeatures(object))))
        }

    }

    invisible(NULL)
})
