#' Signature-based Clustering for Diagnostic Purposes
#'
#' SCUDO is a rank-based method for the analysis of gene expression profiles for
#' diagnostic and classification purposes. It is based on the identification of
#' gene signatures composed of up- and down-regulated genes. Starting from gene
#' expression data, functions in this package identify sample-specific gene
#' signatures and use them to build a graph of samples, that are joined by edges
#' if they have a similar expression profile. The graph can then be used to
#' perform community clustering or to perform supervised classification of
#' samples in a testing set.
#'
#' @seealso \code{\link{scudoTrain}}, \code{\link{scudoNetwork}},
#'   \code{\link{scudoClassify}}
#'
#' @author Matteo Ciciani \email{matteo.ciciani@@gmail.com}, Thomas Cantore
#'   \email{cantorethomas@@gmail.com}
#'
#' @examples
#' example("scudoTrain")
#'
#' @docType package
#' @name rScudo-package
#' @rdname rScudo-package
NULL
