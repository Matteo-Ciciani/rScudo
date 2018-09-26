#' @import methods
NULL

#' Class "scudoResults"
#'
#' This is a class that represents the output of the functions -- link to scudo
#' functions --.
#'
#' This class provides a structure to represent the results of the scudo
#' functions. It contains a distance matrix, i.e. a symmetric matrix with
#' non-negative numeric values.
#'
#' @slot distMatrix Object of class "matrix". A symmetric matrix with
#'   non-negative numeric elements.
#' @slot upSignatures Object of class "data.frame". A data.frame with the same
#'   colnames as distMatrix, representing the up-regualted features in each
#'   sample.
#' @slot downSignatures Object of class "data.frame". A data.frame with the same
#'   colnames as distMatrix, representing the down-regualted features in each
#'   sample.
#' @slot groups Object of class "factor". It represents the groups used for the
#'   normalization and the feature selection. It corresponds to the
#'   \code{groups} argument in --link-- and --link--, but unused levels are
#'   dropped.
#' @slot consensusUpSignatures Object of class "data.frame". It contains the
#'   consensus signatures of up-regulated features for each group.
#' @slot consensusDownSignatures Object of class "data.frame".It contains the
#'   consensus signatures of dowm-regulated features for each group.
#' @slot selectedFeatures Object of class "character". A vector of selected
#'   features. If the feature selection was not performed, it contains every
#'   feature present in the input of the scudo functions.
#' @slot params Object of class "list". A list of the parameters used to run
#'   --link-- or --link--.
#'
#' @section Methods: \describe{ \item{\code{distMatrix}}{\code{signature(object
#'   = "scudoResults")}: a method for obtaining the distance matrix.}
#'   \item{\code{upSignatures}}{\code{signature(object = "scudoResults")}: a
#'   method for obtaining the signature of up-regualted features in each
#'   sample.} \item{\code{downSignatures}}{\code{signature(object =
#'   "scudoResults")}: a method for obtaining the signature of down-regulated
#'   features in each sample.} \item{\code{groups}}{\code{signature(object =
#'   "scudoResults")}: a method for obtaining the groups used for normalization
#'   and feature selection.}
#'   \item{\code{consensusUpSignatures}}{\code{signature(object =
#'   "scudoResults")}: a method for obtaining the consensus signatures of
#'   up-regualted features in each group.}
#'   \item{\code{consensusDownSignatures}}{\code{signature(object =
#'   "scudoResults")}: a method for obtaining the consensus signatures of
#'   down-regulated features in each group.}
#'   \item{\code{selectedFeatures}}{\code{signature(object = "scudoResults")}: a
#'   method for obtaining the names of the features seleted. If no feature
#'   selection was performed, the names of every feature are returned.}
#'   \item{\code{params}}{\code{signature(object = "scudoResults")}: a method
#'   for obtaining the parameters that were used to generate the result.} }
#'
#' @name scudoResults-class
#' @rdname scudoResults-class
#'
#' @export scudoResults
#' @exportClass scudoResults
scudoResults <- setClass("scudoResults",
                        slots = list(
                            distMatrix = "matrix",
                            upSignatures = "data.frame",
                            downSignatures = "data.frame",
                            groups = "factor",
                            consensusUpSignatures = "data.frame",
                            consensusDownSignatures = "data.frame",
                            selectedFeatures = "character",
                            params = "list"))

setValidity("scudoResults", function(object) {
    valid <- TRUE
    msg <- NULL

    # validity of distMatrix ---------------------------------------------------
    if (dim(object@distMatrix)[1] != dim(object@distMatrix)[2]) {
        valid <- FALSE
        msg <- c(msg, "distMatrix is not a square matrix")
    }
    if (any(is.na(object@distMatrix)) & !any(is.nan(object@distMatrix))) {
        valid <- FALSE
        msg <- c(msg, "distMatrix contains NAs")
    }
    if (any(is.nan(object@distMatrix))) {
        valid <- FALSE
        msg <- c(msg, "distMatrix contains NaNs")
    }
    if (!all(is.numeric(object@distMatrix))) {
        valid <- FALSE
        msg <- c(msg, "distMatrix contains non-numeric values")
    }
    if (any(object@distMatrix[!is.na(object@distMatrix)] < 0)) {
        valid <- FALSE
        msg <- c(msg, "distMatrix contains negative numbers")
    }
    if (!isSymmetric(object@distMatrix)) {
        valid <- FALSE
        msg <- c(msg, "distMatrix is not symmetric")
    }
    if (all(is.numeric(object@distMatrix)) &&
        !all(sapply(diag(object@distMatrix),
                    function(x) isTRUE(all.equal(x, 0))))) {
        valid <- FALSE
        msg <- c(msg, "distMatrix contains non-zero elements in the diagonal")
    }
    if (is.null(colnames(object@distMatrix)) |
        is.null(rownames(object@distMatrix))) {
        valid <- FALSE
        msg <- c(msg, "colnames or rownames are not present in distMatrix")
    }
    if (!identical(colnames(object@distMatrix), rownames(object@distMatrix))) {
        valid <- FALSE
        msg <- c(msg, "colnames and rownames are different in distMatrix")
    }

    # validity of upSignatures -------------------------------------------------
    if (any(is.na(object@upSignatures))) {
        valid <- FALSE
        msg <- c(msg, "upSignatures contains NAs")
    }
    if (!all(dim(object@upSignatures)[2] == dim(object@distMatrix))) {
        valid <- FALSE
        msg <- c(msg, paste0("number of columns in upSignatures is different",
                             " from the dimension of distMatrix"))
    }
    if (!identical(colnames(object@distMatrix),
                   colnames(object@upSignatures))) {
        valid <- FALSE
        msg <- c(msg, paste0("colnames in upSignatures are different from",
                             " colnames in distMatrix"))
    }
    if (!all(sapply(object@upSignatures, is.character))) {
        valid <- FALSE
        msg <- c(msg, "upSignatures contains non-character values")
    }

    # validity of downSignatures -----------------------------------------------
    if (any(is.na(object@downSignatures))) {
        valid <- FALSE
        msg <- c(msg, "downSignatures contains NAs")
    }
    if (!all(dim(object@downSignatures)[2] == dim(object@distMatrix))) {
        valid <- FALSE
        msg <- c(msg, paste0("number of columns in downSignatures is different",
                             " from the dimension of distMatrix"))
    }
    if (!identical(colnames(object@distMatrix),
                   colnames(object@downSignatures))) {
        valid <- FALSE
        msg <- c(msg, paste0("colnames in downSignatures are different from",
                             " colnames in distMatrix"))
    }
    if (!all(sapply(object@downSignatures, is.character))) {
        valid <- FALSE
        msg <- c(msg, "downSignatures contains non-character values")
    }

    # validity of groups -------------------------------------------------------
    if (any(is.na(object@groups))) {
        valid <- FALSE
        msg <- c(msg, "groups contains NAs")
    }
    if (length(object@groups) != dim(object@distMatrix)[1] &&
        length(object@groups) != 0) {

        valid <- FALSE
        msg <- c(msg, paste0("length of groups different from number of rows ",
                             "in distMatrix"))
    }

    # validity of consensusUpSignatures ----------------------------------------
    if (!isTRUE(all.equal(dim(object@consensusUpSignatures), c(0, 0)))) {
        if (any(is.na(object@consensusUpSignatures))) {
            valid <- FALSE
            msg <- c(msg, "consensusUpSignatures contains NAs")
        }
        if (dim(object@consensusUpSignatures)[1] != dim(object@upSignatures)[1])
        {
            valid <- FALSE
            msg <- c(msg, paste0("number of rows in consensusUpSignatures ",
                "different from number of rows in upSignatures"))
        }
        if (!all(sapply(object@consensusUpSignatures, is.character))) {
            valid <- FALSE
            msg <- c(msg, "consensusUpSignatures contains non-character values")
        }
        if (!all(is.element(colnames(object@consensusUpSignatures),
                            as.character(levels(object@groups))))) {
            valid <- FALSE
            msg <- c(msg, paste0("colnames of consensusUpSignatures contains ",
                "elements that are not in groups"))
        }
    }

    # validity of consensusDownSignatures --------------------------------------
    if (!isTRUE(all.equal(dim(object@consensusDownSignatures), c(0, 0)))) {
        if (any(is.na(object@consensusDownSignatures))) {
            valid <- FALSE
            msg <- c(msg, "consensusDownSignatures contains NAs")
        }
        if (dim(object@consensusDownSignatures)[1] !=
            dim(object@downSignatures)[1]) {
            valid <- FALSE
            msg <- c(msg, paste0("number of rows in consensusDownSignatures ",
                "different from number of rows in downSignatures"))
        }
        if (!all(sapply(object@consensusDownSignatures, is.character))) {
            valid <- FALSE
            msg <- c(msg, paste0("consensusDownSignatures contains ",
                "non-character values"))
        }
        if (!all(is.element(colnames(object@consensusDownSignatures),
                            as.character(levels(object@groups))))) {
            valid <- FALSE
            msg <- c(msg, paste0("colnames of consensusDownSignatures ",
                "contains elements that are not in groups"))
        }
    }

    # validity of selectedFeatures ---------------------------------------------
    if (any(is.na(object@selectedFeatures))) {
        valid <- FALSE
        msg <- c(msg, "selectedFeatures contains NAs")
    }

    # validity of params -------------------------------------------------------

    if (valid) TRUE else msg
})
