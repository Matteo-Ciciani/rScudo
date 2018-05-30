#' @import methods
#' @include class.R
NULL

#' @rdname ScudoResults-class
#' @aliases DistMatrix,ScudoResults-method
#' @usage NULL
#' @export
setGeneric("DistMatrix", function(object) standardGeneric("DistMatrix"))

#' @rdname ScudoResults-class
#' @aliases DistMatrix,ScudoResults-method
#' @usage NULL
setMethod("DistMatrix", signature = "ScudoResults", definition =
              function(object) object@DistMatrix)

#' @export
setGeneric("UpSignatures", function(object) standardGeneric("UpSignatures"))

setMethod("UpSignatures", signature = "ScudoResults", definition =
              function(object) object@UpSignatures)

#' @export
setGeneric("DownSignatures", function(object) standardGeneric("DownSignatures"))

setMethod("DownSignatures", signature = "ScudoResults", definition =
              function(object) object@DownSignatures)

#' @export
setGeneric("Groups", function(object) standardGeneric("Groups"))

setMethod("Groups", signature = "ScudoResults", definition =
              function(object) object@Groups)

#' @export
setGeneric("ConsensusUpSignatures",
           function(object) standardGeneric("ConsensusUpSignatures"))

setMethod("ConsensusUpSignatures", signature = "ScudoResults", definition =
              function(object) object@ConsensusUpSignatures)

#' @export
setGeneric("ConsensusDownSignatures",
           function(object) standardGeneric("ConsensusDownSignatures"))

setMethod("ConsensusDownSignatures", signature = "ScudoResults", definition =
              function(object) object@ConsensusDownSignatures)

#' @export
setGeneric("SelectedFeatures",
           function(object) standardGeneric("SelectedFeatures"))

setMethod("SelectedFeatures", signature = "ScudoResults", definition =
              function(object) object@SelectedFeatures)

#' @export
setGeneric("Params", function(object) standardGeneric("Params"))

setMethod("Params", signature = "ScudoResults", definition =
              function(object) object@Params)
