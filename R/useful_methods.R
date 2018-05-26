#' @import methods
#' @include class.R accessors.R
NULL

# Show ------------------------------------------------------------------------

#' @export
setMethod("show", "ScudoResults", function(object) {
    cat("Object of ScudoResults\n\n")

    d1 <- dim(DistMatrix(object))
    cat("Distance Matrix \ttot dim:", d1[1], "rows and ", d1[2], "cols\n")
    if (all(d1 > 5)) {
        cat("First 5 rows and cols\n")
        print(DistMatrix(object)[1:5,1:5])
    } else {
        cat("First ", dim(DistMatrix(object))[1], "rows and cols\n")
        print(DistMatrix(object))
    }
    cat("\n")

    d2 <- dim(UpSignatures(object))
    cat("Up Signatures \t\ttot dim:", d2[1], "rows and ", d2[2], "cols\n")
    if (all(d2 > 5)) {
        cat("First 5 rows and cols\n")
        print(UpSignatures(object)[1:5,1:5])
    } else {
        d2_min <- min(d2)
        cat("First ", d2_min, "rows and cols\n")
        print(UpSignatures(object)[1:d2_min,1:d2_min])
    }
    cat("\n")

    d3 <- dim(DownSignatures(object))
    cat("Down Signatures \ttot dim:", d3[1], "rows and ", d3[2], "cols\n")
    if (all(d3 > 5)) {
        cat("First 5 rows and cols\n")
        print(DownSignatures(object)[1:5,1:5])
        cat("\t...\n", DownSignatures(object)[-1,])
    } else {
        d3_min <- min(d3)
        cat("First ", d3_min, "rows and cols\n")
        print(DownSignatures(object)[1:d3_min,1:d3_min])
    }
    cat("\n")

    d4 <- length(SelectedFeatures(object))
    cat("Selected Features \ttot dim:", d4, "\n")
    if (all(d4 > 10)) {
        cat("First 10: ")
        print(SelectedFeatures(object)[1:10])
    } else {
       print(SelectedFeatures(object))
    }

    invisible(NULL)
})


# Summary ----------------------------------------------------------------------

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


# ConsensusSignatures ----------------------------------------------------------

#' @export ConsensusSignatures
#' @title  Consensus Signatures
#'
#' @description If bind = TRUE (default) returns a list containing all Consensus
#' Signatures, else returns a list divided into up and down.
#'
#' @usage \code{ConsensusSignatures(object, ...)}
#'
#' @param object A "ScudoResults" class object
#' @param bind logical, indicating wether or not concatenating Up and Down
#' Singatures
#' @examples
#' ## With 'bind = TRUE' for a generic ScudoResults object
#'
#'ConsensusSignatures(ScudoResObj, bind = TRUE)
#'
#'[1] "l" "m" "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y"
#'
#' ## 'bind = FALSE'
#'
#' $UpSignatures
#'[1] "l" "m" "n" "o" "p" "q" "r"
#'
#'$DownSignatures
#'[1] "s" "t" "u" "v" "w" "x" "y"

ConsensusSignatures <- function(object, bind = TRUE) {
    if (bind) {
        out_con <- c(ConsensusUpSignature(object),
            ConsensusDownSignature(object))
        return(out_con)
    } else {
        return(list("UpSignatures" = ConsensusUpSignature(object),
            "DownSignatures" = ConsensusDownSignature(object)))
    }
}






