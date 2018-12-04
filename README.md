## rScudo: Signature-based Clustering for Diagnostic Purposes

SCUDO (Signature-based Clustering for Diagnostic Purposes) is a rank-based
method for the analysis of gene expression profiles for diagnostic and
classification purposes. It is based on the identification of sample-specific
gene signatures composed of the most up- and down-regulated genes for that
sample. Starting from gene expression data, functions in this package identify
sample-specific gene signatures and use them to build a graph of samples. In
this graph samples are joined by edges if they have a similar expression
profile, according to a pre-computed similarity matrix. The similarity between
the expression profiles of two sample is computed using a method similar to
GSEA. The graph of samples can the be used to perform community clustering or to
perform supervised classification of samples in a testing set.

## Installation

To install this package, install the
[devtools](https://cran.r-project.org/web/packages/devtools/index.html) package
and run:

```
devtools::install_github("Matteo-Ciciani/rScudo")
```

This package requires R >= 3.6.

## Documentation

After installing the package, you can read the vignette for an overview of the
package functionalities:

```
browseVignettes("rScudo")
```
## Bug reports

Bugs should be reported at https://github.com/Matteo-Ciciani/rScudo/issues.

