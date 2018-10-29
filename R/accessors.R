#' @import methods
#' @include class.R
NULL

#' @rdname ScudoResults-class
#' @usage NULL
#' @export
setGeneric("distMatrix", function(object) standardGeneric("distMatrix"))

#' @rdname ScudoResults-class
#' @aliases distMatrix,ScudoResults-method
#' @usage NULL
setMethod("distMatrix", signature = "ScudoResults", definition =
    function(object) object@distMatrix)

#' @rdname ScudoResults-class
#' @usage NULL
#' @export
setGeneric("upSignatures", function(object) standardGeneric("upSignatures"))

#' @rdname ScudoResults-class
#' @aliases upSignatures,ScudoResults-method
#' @usage NULL
setMethod("upSignatures", signature = "ScudoResults", definition =
    function(object) object@upSignatures)

#' @rdname ScudoResults-class
#' @usage NULL
#' @export
setGeneric("downSignatures", function(object) standardGeneric("downSignatures"))

#' @rdname ScudoResults-class
#' @aliases downSignatures,ScudoResults-method
#' @usage NULL
setMethod("downSignatures", signature = "ScudoResults", definition =
    function(object) object@downSignatures)

#' @rdname ScudoResults-class
#' @usage NULL
#' @export
setGeneric("groupsAnnotation", function(object) standardGeneric(
    "groupsAnnotation"))

#' @rdname ScudoResults-class
#' @aliases groupsAnnotation,ScudoResults-method
#' @usage NULL
setMethod("groupsAnnotation", signature = "ScudoResults", definition =
    function(object) object@groupsAnnotation)

#' @rdname ScudoResults-class
#' @usage NULL
#' @export
setGeneric("consensusUpSignatures",
    function(object) standardGeneric("consensusUpSignatures"))

#' @rdname ScudoResults-class
#' @aliases consensusUpSignatures,ScudoResults-method
#' @usage NULL
setMethod("consensusUpSignatures", signature = "ScudoResults", definition =
    function(object) object@consensusUpSignatures)

#' @rdname ScudoResults-class
#' @usage NULL
#' @export
setGeneric("consensusDownSignatures",
    function(object) standardGeneric("consensusDownSignatures"))

#' @rdname ScudoResults-class
#' @aliases consensusDownSignatures,ScudoResults-method
#' @usage NULL
setMethod("consensusDownSignatures", signature = "ScudoResults", definition =
    function(object) object@consensusDownSignatures)

#' @rdname ScudoResults-class
#' @usage NULL
#' @export
setGeneric("selectedFeatures",
    function(object) standardGeneric("selectedFeatures"))

#' @rdname ScudoResults-class
#' @aliases selectedFeatures,ScudoResults-method
#' @usage NULL
setMethod("selectedFeatures", signature = "ScudoResults", definition =
    function(object) object@selectedFeatures)

#' @rdname ScudoResults-class
#' @usage NULL
#' @export
setGeneric("scudoParams", function(object) standardGeneric("scudoParams"))

#' @rdname ScudoResults-class
#' @aliases scudoParams,ScudoResults-method
#' @usage NULL
setMethod("scudoParams", signature = "ScudoResults", definition =
    function(object) object@scudoParams)
