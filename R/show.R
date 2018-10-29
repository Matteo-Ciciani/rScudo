#' @import methods
#' @include class.R accessors.R
NULL

# Show -------------------------------------------------------------------------

#' @rdname ScudoResults-class
#' @aliases show,ScudoResults-method
#' @usage NULL
setMethod("show", "ScudoResults", function(object) {
    cat("Object of class ScudoResults\n")
    cat("Result of", ifelse(length(scudoParams(object)) == 4,
        "scudoTest", "scudoTrain"), "\n\n")
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

    cat("upSignatures length    : ", paste(scudoParams(object)$nTop),
        "\n")
    cat("downSignatures length  : ", paste(scudoParams(object)$nBottom),
        "\n")
    cat("Fold-changes           : ", paste0(ifelse(
        scudoParams(object)$foldChange, "", "not "), "computed"), "\n")
    if (scudoParams(object)$foldChange) {
        cat("    grouped            : ", ifelse(scudoParams(
            object)$groupedFoldChange, "Yes", "No "), "\n")
    }

    if (length(scudoParams(object)) == 8 && scudoParams(object)$featureSel) {
        cat("Feature selection      :  performed\n")
        if (scudoParams(object)$parametric) {
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
        cat("    p-value cutoff     : ", paste(scudoParams(object)$alpha), "\n")
        cat("    p.adjust method    : ", paste(scudoParams(object)$pAdj), "\n")
        cat("    Selected features  : ", paste(length(selectedFeatures(
            object))))
    } else  if (length(scudoParams(object)) == 8) {
        cat("Feature selection      :  not performed\n")
    }

    invisible(NULL)
})
