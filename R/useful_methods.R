#' @import methods
#' @include class.R accessors.R
NULL

<<<<<<< HEAD
# show
# Summary ---------------------------------------------------------------------

=======
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
>>>>>>> f91acbf580a7002a470f97f6ed7b1eeea4541245


# ConsensusSignatures: returns a list of ConsensusUp.. and Down

<<<<<<< HEAD
=======

>>>>>>> f91acbf580a7002a470f97f6ed7b1eeea4541245
