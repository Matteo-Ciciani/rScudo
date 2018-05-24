#' @import methods
#' @include class.R
NULL

#' @export
setGeneric("DistMatrix", function(object) standardGeneric("DistMatrix"))

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
setGeneric("ConsensusUpSignature",
           function(object) standardGeneric("ConsensusUpSignature"))

setMethod("ConsensusUpSignature", signature = "ScudoResults", definition =
              function(object) object@ConsensusUpSignature)

#' @export
setGeneric("ConsensusDownSignature",
           function(object) standardGeneric("ConsensusDownSignature"))

setMethod("ConsensusDownSignature", signature = "ScudoResults", definition =
              function(object) object@ConsensusDownSignature)

#' @export
setGeneric("SelectedFeatures",
           function(object) standardGeneric("SelectedFeatures"))

setMethod("SelectedFeatures", signature = "ScudoResults", definition =
              function(object) object@SelectedFeatures)

#' @export
setGeneric("Params", function(object) standardGeneric("Params"))

setMethod("Params", signature = "ScudoResults", definition =
              function(object) object@Params)

