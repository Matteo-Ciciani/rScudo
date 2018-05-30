#' @import methods
NULL

#' Class "ScudoResults"
#'
#' This is a class that represents the output of the functions
#' -- link to scudo functions --.
#'
#' Details about the class
#'
#' @slot DistMatrix Object of class "matrix".
#' @slot UpSignatures Object of class "data.frame".
#' @slot DownSignatures Object of class "data.frame."
#' @slot Groups Object of class "factor".
#' @slot ConsensusUpSignatures Object of class "data.frame".
#' @slot ConsensusDownSignatures Object of class "data.frame".
#' @slot SelectedFeatures Object of class "character".
#' @slot Params Object of class "list".
#'
#' @section Methods:
#' \describe{
#'   \item{One}{First item}
#'   \item{Two}{Second item}
#' }
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
