#' @import methods
#' @export ScudoResults
#' @exportClass ScudoResults
ScudoResults <- setClass("ScudoResults",
                        slots = list(
                            DistMatrix = "matrix",
                            UpSignatures = "data.frame",
                            DownSignatures = "data.frame",
                            ConsensusUpSignature = "character",
                            ConsensusDownSignature = "character",
                            SelectedFeatures = "character",
                            Params = "list"))

setValidity("ScudoResults", function(object) {
    valid <- TRUE
    msg <- NULL
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
    if (is.null(colnames(object@DistMatrix)) |
        is.null(rownames(object@DistMatrix))) {
        valid <- FALSE
        msg <- c(msg, "colnames or rownames are not present in DistMatrix")
    }
    if (!identical(colnames(object@DistMatrix), rownames(object@DistMatrix))) {
        valid <- FALSE
        msg <- c(msg, "colnames and rownames are different in DistMatrix")
    }
    if (any(is.na(object@UpSignatures))) {
        valid <- FALSE
        msg <- c(msg, "UpSignatures contains NAs")
    }
    if (!all(dim(object@UpSignatures)[2] == dim(object@DistMatrix))) {
        valid <- FALSE
        msg <- c(msg, "Number of columns in UpSignatures is different from the
                 dimension of DistMatrix")
    }
    if (!identical(colnames(object@DistMatrix),
                   colnames(object@UpSignatures))) {
        valid <- FALSE
        msg <- c(msg, "colnames in UpSignatures are different from colnames in
                 DistMatrix")
    }
    if (!all(is.character(as.matrix(object@UpSignatures)))) {
        valid <- FALSE
        msg <- c(msg, "UpSignatures contains non-character values")
    }
    if (any(is.na(object@DownSignatures))) {
        valid <- FALSE
        msg <- c(msg, "DownSignatures contains NAs")
    }
    if (!all(dim(object@DownSignatures)[2] == dim(object@DistMatrix))) {
        valid <- FALSE
        msg <- c(msg, "Number of columns in DownSignatures is different from the
                 dimension of DistMatrix")
    }
    if (!identical(colnames(object@DistMatrix),
                   colnames(object@DownSignatures))) {
        valid <- FALSE
        msg <- c(msg, "colnames in DownSignatures are different from colnames in
                 DistMatrix")
    }
    if (!all(is.character(as.matrix(object@DownSignatures)))) {
        valid <- FALSE
        msg <- c(msg, "DownSignatures contains non-character values")
    }
    if (any(is.na(object@ConsensusUpSignature))) {
        valid <- FALSE
        msg <- c(msg, "ConsensusUpSignature contains NAs")
    }
    if (length(object@ConsensusUpSignature) != dim(object@UpSignatures)[1]) {
        valid <- FALSE
        msg <- c(msg, "ConsensusUpSignature different from number of rows in
                 UpSignatures")
    }
    if (any(is.na(object@ConsensusDownSignature))) {
        valid <- FALSE
        msg <- c(msg, "ConsensusDownSignature contains NAs")
    }
    if (length(object@ConsensusDownSignature) !=
        dim(object@DownSignatures)[1]) {
        valid <- FALSE
        msg <- c(msg, "ConsensusDownSignature different from number of rows in
                 DownSignatures")
    }
    if (any(is.na(object@SelectedFeatures))) {
        valid <- FALSE
        msg <- c(msg, "SelectedFeatures contains NAs")
    }
    # check Params
    if (valid) TRUE else msg
})
