#' @import methods
#' @include class.R accessors.R
NULL

# Show ------------------------------------------------------------------------

#' @export
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
#ADD n NUMBER OF SAMPLES
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


# ConsensusSignatures: returns a list of ConsensusUp.. and Down
#' @export
setGeneric("ConsensusSignatures",
           function(object) standardGeneric("ConsensusSignatures"))

setMethod("ConsensusSignatures", signature = "ScudoResults", definition =
              function(object) {
                  cat("The upper consensus signature is:\n\t",
                      ConsensusUpSignature(object),
                      "\nThe lower consensus signature is:\n\t",
                      ConsensusDownSignature(object))})


