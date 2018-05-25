#' @import methods
#' @include class.R accessors.R
NULL

# Show ------------------------------------------------------------------------

#' @export
setMethod("show", "ScudoResults", function(object) {
    cat("Object of ScudoResults\n\n")

    d1 <- dim(object@DistMatrix)
    cat("Distance Matrix \ttot dim:", d1[1], "rows and ", d1[2], "cols\n")
    if (all(d1 > 5)) {
        cat("First 5 rows and cols\n")
        print(object@DistMatrix[1:5,1:5])
    } else {
        cat("First ", dim(object@DistMatrix)[1], "rows and cols\n")
        print(object@DistMatrix)
    }
    cat("\n")

    d2 <- dim(object@UpSignatures)
    cat("Up Signatures \t\ttot dim:", d2[1], "rows and ", d2[2], "cols\n")
    if (all(d2 > 5)) {
        cat("First 5 rows and cols\n")
        print(object@UpSignatures[1:5,1:5])
    } else {
        d2_min <- min(d2)
        cat("First ", d2_min, "rows and cols\n")
        print(object@UpSignatures[1:d2_min,1:d2_min])
    }
    cat("\n")

    d3 <- dim(object@DownSignatures)
    cat("Down Signatures \ttot dim:", d3[1], "rows and ", d3[2], "cols\n")
    if (all(d3 > 5)) {
        cat("First 5 rows and cols\n")
        print(DownSignatures(object)[1:5,1:5])
        cat("\t...\n", object@DownSignatures[-1,])
    } else {
        d3_min <- min(d3)
        cat("First ", d3_min, "rows and cols\n")
        print(object@DownSignatures[1:d3_min,1:d3_min])
    }
    cat("\n")

    d4 <- length(object@SelectedFeatures)
    cat("Selected Features \ttot dim:", d4, "\n")
    if (all(d4 > 10)) {
        cat("First 10: ")
        print(ScudoRes@SelectedFeatures[1:10])
    } else {
       print(object@SelectedFeatures)
    }

    invisible(NULL)
})


# Summary ---------------------------------------------------------------------



# ConsensusSignatures: returns a list of ConsensusUp.. and Down


