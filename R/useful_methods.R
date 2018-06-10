#' @import methods
#' @include class.R accessors.R
NULL

# Show -------------------------------------------------------------------------


setMethod("show", "ScudoResults", function(object) {
    cat("Scudo Results Object",  "\n\n")
    cat("Class                  : ", class(object), "\n")
    cat("Number of samples      : ", paste(dim(DistMatrix(object))[1]),
        "\n")
    cat("UpSignatures           : ", paste(Params(object)$nTop),
        "\n")
    cat("DownSignatures         : ", paste(Params(object)$nBottom),
        "\n")

    nGroups <- length(colnames(ConsensusDownSignatures(object)))
    cat("Number of groups       : ", nGroups, "\n")

    if (length(Params(object)) == 3) {

        cat("ScudoPredict           : ", "Performed\n")
        cat("Normalization            ", paste(Params(object)$prepro), "\n")

    }else{

        cat("Normalization          : ", paste(Params(object)$prepro),
            "\n")
        cat("Feature selection        ", "\n")
        cat("  Performed            : ", Params(object)$featureSel, "\n")

        if (Params(object)$featureSel) {
            if (nGroups == 2) {
                cat("  Test                 : ",
                    "Wilcoxon Rank Sum statistic", "\n")
            }else{
                cat("  Test                 : ",
                    "Kruskal-Wallis Rank Sum Test", "\n")
            }
            cat("  pValue               : ", paste(Params(object)$pValue),
                "\n")
            cat("  p.adj method         : ", paste(Params(object)$p.adj),
                "\n")
            cat("  selected features    : ",
                paste(length(SelectedFeatures(object))))
        }

    }

    invisible(NULL)
})


# Summary ----------------------------------------------------------------------

#' @export
setMethod("summary", signature = "ScudoResults", definition =
              function(object) {
                  cat("The analysis is performed on", n, "samples",
                      "\nThe upper signatures are made of",
                      dim(UpSignatures(object))[1], "genes",
                      "\nThe upper signatures are made of",
                      dim(DownSignatures(object))[1], "genes",
                      "\nThe consensus sequence is:\n\t",
                      c(ConsensusUpSignature(object),
                        ConsensusDownSignature(object)))})
