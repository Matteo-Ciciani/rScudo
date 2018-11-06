#' Signature-based Clustering for Diagnostic Purposes
#'
#' SCUDO (Signature-based Clustering for Diagnostic Purposes) is a rank-based
#' method for the analysis of gene expression profiles for diagnostic and
#' classification purposes. It is based on the identification of sample-specific
#' gene signatures composed of the most up- and down-regulated genes for that
#' sample. Starting from gene expression data, functions in this package
#' identify sample-specific gene signatures and use them to build a graph of
#' samples. In this graph samples are joined by edges if they have a similar
#' expression profile, according to a pre-computed similarity matrix. The
#' similarity between the expression profile of two sample is computed using a
#' method similar to GSEA. The graph of samples can the be used to perform
#' community clustering or to perform supervised classification of samples in a
#' testing set.
#'
#' @seealso \code{\link{scudoTrain}}, \code{\link{scudoNetwork}},
#' \code{\link{scudoClassify}}
#'
#' @author Matteo Ciciani \email{matteo.ciciani@@gmail.com}, Thomas Cantore
#' \email{cantorethomas@@gmail.com}
#'
#' @examples
#' # To learn more about rScudo, start with the vignette:
#' browseVignettes("rScudo")
#'
#' @docType package
#' @name rScudo-package
#' @rdname rScudo-package
NULL
