#' @import methods
#' @include class.R accessors.R
NULL

# Show ------------------------------------------------------------------------

setMethod("show", "ScudoResults", function(object) {
    cat("Object of ScudoResults\n\n")

    cat("Distance Matrix\n")
    print(object@DistMatrix[1:5,1:5])
    cat("\n")

    cat("Up Signatures\n")
    print(object@UpSignatures[1:5,1:5])
    cat("\n")

    cat("Down Signatures\n")
    print(object@DownSignatures[1:5,1:5])
    cat("\n")

    cat("Selected Features\n", object@SelectedFeatures,
        "\n\n")

    invisible(NULL)
})

# Summary ---------------------------------------------------------------------



# ConsensusSignatures: returns a list of ConsensusUp.. and Down


