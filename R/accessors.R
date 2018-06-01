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

#' @rdname ScudoResults-class
#' @aliases UpSignatures,ScudoResults-method
#' @usage NULL
#' @export
setGeneric("UpSignatures", function(object) standardGeneric("UpSignatures"))

#' @rdname ScudoResults-class
#' @aliases UpSignatures,ScudoResults-method
#' @usage NULL
setMethod("UpSignatures", signature = "ScudoResults", definition =
              function(object) object@UpSignatures)

#' @rdname ScudoResults-class
#' @aliases DownSignatures,ScudoResults-method
#' @usage NULL
#' @export
setGeneric("DownSignatures", function(object) standardGeneric("DownSignatures"))

#' @rdname ScudoResults-class
#' @aliases DownSignatures,ScudoResults-method
#' @usage NULL
setMethod("DownSignatures", signature = "ScudoResults", definition =
              function(object) object@DownSignatures)

#' @rdname ScudoResults-class
#' @aliases Groups,ScudoResults-method
#' @usage NULL
#' @export
setGeneric("Groups", function(object) standardGeneric("Groups"))

#' @rdname ScudoResults-class
#' @aliases Groups,ScudoResults-method
#' @usage NULL
setMethod("Groups", signature = "ScudoResults", definition =
              function(object) object@Groups)

#' @rdname ScudoResults-class
#' @aliases ConsensusUpSignatures,ScudoResults-method
#' @usage NULL
#' @export
setGeneric("ConsensusUpSignatures",
           function(object) standardGeneric("ConsensusUpSignatures"))

#' @rdname ScudoResults-class
#' @aliases Groups,ScudoResults-method
#' @usage NULL
setMethod("ConsensusUpSignatures", signature = "ScudoResults", definition =
              function(object) object@ConsensusUpSignatures)

#' @rdname ScudoResults-class
#' @aliases ConsensusDownSignatures,ScudoResults-method
#' @usage NULL
#' @export
setGeneric("ConsensusDownSignatures",
           function(object) standardGeneric("ConsensusDownSignatures"))

#' @rdname ScudoResults-class
#' @aliases ConsensusDownSignatures,ScudoResults-method
#' @usage NULL
setMethod("ConsensusDownSignatures", signature = "ScudoResults", definition =
              function(object) object@ConsensusDownSignatures)

#' @rdname ScudoResults-class
#' @aliases SelectedFeatures,ScudoResults-method
#' @usage NULL
#' @export
setGeneric("SelectedFeatures",
           function(object) standardGeneric("SelectedFeatures"))

#' @rdname ScudoResults-class
#' @aliases SelectedFeatures,ScudoResults-method
#' @usage NULL
setMethod("SelectedFeatures", signature = "ScudoResults", definition =
              function(object) object@SelectedFeatures)

#' @rdname ScudoResults-class
#' @aliases Params,ScudoResults-method
#' @usage NULL
#' @export
setGeneric("Params", function(object) standardGeneric("Params"))

#' @rdname ScudoResults-class
#' @aliases Params,ScudoResults-method
#' @usage NULL
setMethod("Params", signature = "ScudoResults", definition =
              function(object) object@Params)
