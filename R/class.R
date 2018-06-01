#' @import methods
NULL

#' Class "ScudoResults"
#'
#' This is a class that represents the output of the functions -- link to scudo
#' functions --.
#'
#' This class provides a structure to represent the results of the scudo
#' functions. It contains a distance matrix, i.e. a symmetric matrix with
#' non-negative numeric values.
#'
#' @slot DistMatrix Object of class "matrix". A symmetric matrix with
#'   non-negative numeric elements.
#' @slot UpSignatures Object of class "data.frame". A data.frame with the same
#'   colnames as DistMatrix, representing the up-regualted features in each
#'   sample.
#' @slot DownSignatures Object of class "data.frame". A data.frame with the same
#'   colnames as DistMatrix, representing the down-regualted features in each
#'   sample.
#' @slot Groups Object of class "factor". It represents the groups used for the
#'   normalization and the feature selection. It corresponds to the
#'   \code{groups} argument in --link-- and --link--, but unused levels are
#'   dropped.
#' @slot ConsensusUpSignatures Object of class "data.frame". It contains the
#'   consensus signatures of up-regulated features for each group.
#' @slot ConsensusDownSignatures Object of class "data.frame".It contains the
#'   consensus signatures of dowm-regulated features for each group.
#' @slot SelectedFeatures Object of class "character". A vector of selected
#'   features. If the feature selection was not performed, it contains every
#'   feature present in the input of the scudo functions.
#' @slot Params Object of class "list". A list of the parameters used to run
#'   --link-- or --link--.
#'
#' @section Methods: \describe{ \item{\code{DistMatrix}}{\code{signature(object
#'   = "ScudoResults")}: a method for obtaining the distance matrix.}
#'   \item{\code{UpSignatures}}{\code{signature(object = "ScudoResults")}: a
#'   method for obtaining the signature of up-regualted features in each
#'   sample.} \item{\code{DownSignatures}}{\code{signature(object =
#'   "ScudoResults")}: a method for obtaining the signature of down-regulated
#'   features in each sample.} \item{\code{Groups}}{\code{signature(object =
#'   "ScudoResults")}: a method for obtaining the groups used for normalization
#'   and feature selection.}
#'   \item{\code{ConsensusUpSignatures}}{\code{signature(object =
#'   "ScudoResults")}: a method for obtaining the consensus signatures of
#'   up-regualted features in each group.}
#'   \item{\code{ConsensusDownSignatures}}{\code{signature(object =
#'   "ScudoResults")}: a method for obtaining the consensus signatures of
#'   down-regulated features in each group.}
#'   \item{\code{SelectedFeatures}}{\code{signature(object = "ScudoResults")}: a
#'   method for obtaining the names of the features seleted. If no feature
#'   selection was performed, the names of every feature are returned.}
#'   \item{\code{Params}}{\code{signature(object = "ScudoResults")}: a method
#'   for obtaining the parameters that were used to generate the result.} }
#'
#' @name ScudoResults-class
#' @rdname ScudoResults-class
#'
#' @export ScudoResults
#' @exportClass ScudoResults
ScudoResults <- setClass("ScudoResults",
                        slots = list(
                            DistMatrix = "matrix",
                            UpSignatures = "data.frame",
                            DownSignatures = "data.frame",
                            Groups = "factor",
                            ConsensusUpSignatures = "data.frame",
                            ConsensusDownSignatures = "data.frame",
                            SelectedFeatures = "character",
                            Params = "list"))

setValidity("ScudoResults", function(object) {
    valid <- TRUE
    msg <- NULL

    # validity of DistMatrix ---------------------------------------------------
    if (dim(object@DistMatrix)[1] != dim(object@DistMatrix)[2]) {
        valid <- FALSE
        msg <- c(msg, "DistMatrix is not a square matrix")
    }
    if (any(is.na(object@DistMatrix)) & !any(is.nan(object@DistMatrix))) {
        valid <- FALSE
        msg <- c(msg, "DistMatrix contains NAs")
    }
    if (any(is.nan(object@DistMatrix))) {
        valid <- FALSE
        msg <- c(msg, "DistMatrix contains NaNs")
    }
    if (!all(is.numeric(object@DistMatrix))) {
        valid <- FALSE
        msg <- c(msg, "DistMatrix contains non-numeric values")
    }
    if (any(object@DistMatrix[!is.na(object@DistMatrix)] < 0)) {
        valid <- FALSE
        msg <- c(msg, "DistMatrix contains negative numbers")
    }
    if (!isSymmetric(object@DistMatrix)) {
        valid <- FALSE
        msg <- c(msg, "DistMatrix is not symmetric")
    }
    if (all(is.numeric(object@DistMatrix)) &&
        !all(sapply(diag(object@DistMatrix),
                    function(x) isTRUE(all.equal(x, 0))))) {
        valid <- FALSE
        msg <- c(msg, "DistMatrix contains non-zero elements in the diagonal")
    }
    if (is.null(colnames(object@DistMatrix)) |
        is.null(rownames(object@DistMatrix))) {
        valid <- FALSE
        msg <- c(msg, "colnames or rownames are not present in DistMatrix")
    }
    if (!identical(colnames(object@DistMatrix), rownames(object@DistMatrix))) {
        valid <- FALSE
        msg <- c(msg, "colnames and rownames are different in DistMatrix")
    }

    # validity of UpSignatures -------------------------------------------------
    if (any(is.na(object@UpSignatures))) {
        valid <- FALSE
        msg <- c(msg, "UpSignatures contains NAs")
    }
    if (!all(dim(object@UpSignatures)[2] == dim(object@DistMatrix))) {
        valid <- FALSE
        msg <- c(msg, paste0("number of columns in UpSignatures is different",
                             " from the dimension of DistMatrix"))
    }
    if (!identical(colnames(object@DistMatrix),
                   colnames(object@UpSignatures))) {
        valid <- FALSE
        msg <- c(msg, paste0("colnames in UpSignatures are different from",
                             " colnames in DistMatrix"))
    }
    if (!all(sapply(object@UpSignatures, is.character))) {
        valid <- FALSE
        msg <- c(msg, "UpSignatures contains non-character values")
    }

    # validity of DownSignatures -----------------------------------------------
    if (any(is.na(object@DownSignatures))) {
        valid <- FALSE
        msg <- c(msg, "DownSignatures contains NAs")
    }
    if (!all(dim(object@DownSignatures)[2] == dim(object@DistMatrix))) {
        valid <- FALSE
        msg <- c(msg, paste0("number of columns in DownSignatures is different",
                             " from the dimension of DistMatrix"))
    }
    if (!identical(colnames(object@DistMatrix),
                   colnames(object@DownSignatures))) {
        valid <- FALSE
        msg <- c(msg, paste0("colnames in DownSignatures are different from",
                             " colnames in DistMatrix"))
    }
    if (!all(sapply(object@DownSignatures, is.character))) {
        valid <- FALSE
        msg <- c(msg, "DownSignatures contains non-character values")
    }

    # validity of Groups -------------------------------------------------------
    if (any(is.na(object@Groups))) {
        valid <- FALSE
        msg <- c(msg, "Groups contains NAs")
    }
    if (length(object@Groups) != dim(object@DistMatrix)[1]) {
        valid <- FALSE
        msg <- c(msg, paste0("length of Groups different from number of rows ",
                             "in DistMatrix"))
    }

    # validity of ConsensusUpSignatures ----------------------------------------
    if (any(is.na(object@ConsensusUpSignatures))) {
        valid <- FALSE
        msg <- c(msg, "ConsensusUpSignatures contains NAs")
    }
    if (dim(object@ConsensusUpSignatures)[1] != dim(object@UpSignatures)[1]) {
        valid <- FALSE
        msg <- c(msg, paste0("number of rows in ConsensusUpSignatures ",
                             "different from number of rows in UpSignatures"))
    }
    if (!all(sapply(object@ConsensusUpSignatures, is.character))) {
        valid <- FALSE
        msg <- c(msg, "ConsensusUpSignatures contains non-character values")
    }
    if (!all(is.element(colnames(object@ConsensusUpSignatures),
                        as.character(levels(object@Groups))))) {
        valid <- FALSE
        msg <- c(msg, paste0("colnames of ConsensusUpSignatures contains ",
                             "elements that are not in Groups"))
    }

    # validity of ConsensusDownSignatures --------------------------------------
    if (any(is.na(object@ConsensusDownSignatures))) {
        valid <- FALSE
        msg <- c(msg, "ConsensusDownSignatures contains NAs")
    }
    if (dim(object@ConsensusDownSignatures)[1] !=
        dim(object@DownSignatures)[1]) {
        valid <- FALSE
        msg <- c(msg, paste0("number of rows in ConsensusDownSignatures ",
                             "different from number of rows in DownSignatures"))
    }
    if (!all(sapply(object@ConsensusDownSignatures, is.character))) {
        valid <- FALSE
        msg <- c(msg, "ConsensusDownSignatures contains non-character values")
    }
    if (!all(is.element(colnames(object@ConsensusDownSignatures),
                        as.character(levels(object@Groups))))) {
        valid <- FALSE
        msg <- c(msg, paste0("colnames of ConsensusDownSignatures contains ",
                             "elements that are not in Groups"))
    }

    # validity of SelectedFeatures ---------------------------------------------
    if (any(is.na(object@SelectedFeatures))) {
        valid <- FALSE
        msg <- c(msg, "SelectedFeatures contains NAs")
    }

    # validity of Params -------------------------------------------------------

    if (valid) TRUE else msg
})
