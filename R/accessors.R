#' @import methods
#' @include class.R
NULL

#' @rdname scudoResults-class
#' @usage NULL
#' @export
setGeneric("distMatrix", function(object) standardGeneric("distMatrix"))

#' @rdname scudoResults-class
#' @aliases distMatrix,scudoResults-method
#' @usage NULL
setMethod("distMatrix", signature = "scudoResults", definition =
    function(object) object@distMatrix)

#' @rdname scudoResults-class
#' @usage NULL
#' @export
setGeneric("upSignatures", function(object) standardGeneric("upSignatures"))

#' @rdname scudoResults-class
#' @aliases upSignatures,scudoResults-method
#' @usage NULL
setMethod("upSignatures", signature = "scudoResults", definition =
    function(object) object@upSignatures)

#' @rdname scudoResults-class
#' @usage NULL
#' @export
setGeneric("downSignatures", function(object) standardGeneric("downSignatures"))

#' @rdname scudoResults-class
#' @aliases downSignatures,scudoResults-method
#' @usage NULL
setMethod("downSignatures", signature = "scudoResults", definition =
    function(object) object@downSignatures)

#' @rdname scudoResults-class
#' @usage NULL
#' @export
setGeneric("groupsAnnotation", function(object) standardGeneric(
    "groupsAnnotation"))

#' @rdname scudoResults-class
#' @aliases groupsAnnotation,scudoResults-method
#' @usage NULL
setMethod("groupsAnnotation", signature = "scudoResults", definition =
    function(object) object@groupsAnnotation)

#' @rdname scudoResults-class
#' @usage NULL
#' @export
setGeneric("consensusUpSignatures",
    function(object) standardGeneric("consensusUpSignatures"))

#' @rdname scudoResults-class
#' @aliases consensusUpSignatures,scudoResults-method
#' @usage NULL
setMethod("consensusUpSignatures", signature = "scudoResults", definition =
    function(object) object@consensusUpSignatures)

#' @rdname scudoResults-class
#' @usage NULL
#' @export
setGeneric("consensusDownSignatures",
    function(object) standardGeneric("consensusDownSignatures"))

#' @rdname scudoResults-class
#' @aliases consensusDownSignatures,scudoResults-method
#' @usage NULL
setMethod("consensusDownSignatures", signature = "scudoResults", definition =
    function(object) object@consensusDownSignatures)

#' @rdname scudoResults-class
#' @usage NULL
#' @export
setGeneric("selectedFeatures",
    function(object) standardGeneric("selectedFeatures"))

#' @rdname scudoResults-class
#' @aliases selectedFeatures,scudoResults-method
#' @usage NULL
setMethod("selectedFeatures", signature = "scudoResults", definition =
    function(object) object@selectedFeatures)

#' @rdname scudoResults-class
#' @usage NULL
#' @export
setGeneric("params", function(object) standardGeneric("params"))

#' @rdname scudoResults-class
#' @aliases params,scudoResults-method
#' @usage NULL
setMethod("params", signature = "scudoResults", definition =
    function(object) object@params)
