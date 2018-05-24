#' @import methods
#' @include class.R accessors.R
NULL

# Show function ----------------------------------------------------------------

setMethod("show", "ScudoResults", function(object) {
    cat("Object of", class(object),"\n\n")

    cat("Distance Matrix\n")
    print(object@DistMatrix1:5,1:5])
    cat("\n\n")

    cat("Up Signatures\n")
    print(object@UpSignatures[1:5,1:5])
    cat("\n\n")

    cat("Down Signatures\n")
    print(object@DownSignatures[1:5,1:5])
    cat("\n\n")

    cat("Consensus Up Signature\n", object@ConsensusUpSignature,
        "\n\n")
    cat("Consensus Down Signature\n", object@ConsensusDownSignature,
        "\n\n")
    cat("Selected Features\n", object@SelectedFeatures,
        "\n\n")
    cat("Parameters\n", object@Params,
        "\n\n")
})

# summary

# ConsensusSignatures: returns a list of ConsensusUp.. and Down

