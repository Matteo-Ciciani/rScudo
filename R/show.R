#' @import methods
#' @include class.R accessors.R
NULL

# Show -------------------------------------------------------------------------

#' @rdname scudoResults-class
#' @aliases show,scudoResults-method
#' @usage NULL
setMethod("show", "scudoResults", function(object) {
    cat("Object of class scudoResults\n")
    cat("Result of", ifelse(length(params(object)) == 4,
        "scudoTest", "scudo"), "\n\n")
    cat("Number of samples      : ", paste(dim(distMatrix(object))[1]),
        "\n")

    ngroups <- ncol(consensusDownSignatures(object))
    if (ngroups != 0) {
        cat("Number of groups       : ", ngroups, "\n")
        gt <- table(groupsAnnotation(object)[, drop = TRUE])
        for (x in names(gt)) cat("   ", x, ": ", gt[x], "samples\n")
    } else {
        cat("No groups specified\n")
    }

    cat("upSignatures length    : ", paste(params(object)$nTop),
        "\n")
    cat("downSignatures length  : ", paste(params(object)$nBottom),
        "\n")
    cat("Fold-changes           : ", paste0(ifelse(params(object)$foldChange,
        "", "not "), "computed"), "\n")
    if (params(object)$foldChange) {
        cat("    grouped            : ", ifelse(params(
            object)$groupedFoldChange, "Yes", "No "), "\n")
    }

    if (length(params(object)) == 8 && params(object)$featureSel) {
        cat("Feature selection      :  performed\n")
        if (params(object)$parametric) {
            if (ngroups == 2) {
                cat("    Test               :  Student's t-test\n")
            } else {
                cat("    Test               :  anova test\n")
            }
        } else {
            if (ngroups == 2) {
                cat("    Test               :  Wilcoxon rank sum test\n")
            } else {
                cat("    Test               :  Kruskal-Wallis rank sum test\n")
            }
        }
        cat("    p-value cutoff     : ", paste(params(object)$alpha), "\n")
        cat("    p.adjust method    : ", paste(params(object)$pAdj), "\n")
        cat("    Selected features  : ", paste(length(selectedFeatures(
            object))))
    } else  if (length(params(object)) == 8) {
        cat("Feature selection      :  not performed\n")
    }

    invisible(NULL)
})
