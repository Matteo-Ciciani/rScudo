#' @import methods
#' @include class.R accessors.R
NULL

# show
# summary
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


# ConsensusSignatures: returns a list of ConsensusUp.. and Down

