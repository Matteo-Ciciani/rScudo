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
        "scudoTest", "scudo"), "\n\n")
    cat("Number of samples      : ", paste(dim(distMatrix(object))[1]),
        "\n")

    ngroups <- length(colnames(consensusDownSignatures(object)))
    if (ngroups != 1) {
        cat("Number of groups       : ", ngroups, "\n")
        gt <- table(groups(object)[, drop = TRUE])
        invisible(sapply(names(gt), function(x) cat("   ", x, ": ", gt[x],
            "samples\n")))
    }

    cat("upSignatures length    : ", paste(params(object)$nTop),
        "\n")
    cat("downSignatures length  : ", paste(params(object)$nBottom),
        "\n")
    cat("Normalization          : ", paste0(ifelse(params(object)$norm,
        "", "not "), "performed"), "\n")

    featureSel <- params(object)$featureSel

    if (length(params(object)) != 8) {
        if (is.null(featureSel)) featureSel <- F
        cat("Feature selection      : ", paste0(
            (if (featureSel) "" else "not "),
            "performed"), "\n")

     } else {
        if (featureSel) {
            cat("Feature selection      :  performed\n")
            if (params(object)$parametric == T) {
                if (ngroups == 2) {
                    cat("    Test               : ",
                    "t-test", "\n")
                } else {
                    cat("    Test               : ",
                    "anova test", "\n")
                }
            } else {
                if (ngroups == 2) {
                    cat("    Test               : ",
                        "Wilcoxon rank sum test", "\n")
                } else {
                    cat("    Test               : ",
                        "Kruskal-Wallis rank sum test", "\n")
                }
            }

            cat("    p-value cutoff     : ",
                paste(params(object)$pValue), "\n")
            cat("    p.adjust method    : ",
                paste(params(object)$pAdj), "\n")
            cat("    Selected features  : ",
                paste(length(selectedFeatures(object))))
        }

    }
    invisible(NULL)
})
