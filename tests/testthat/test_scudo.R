context("Test scudo")
library(scudo)

test_that("scudo raises errors on wrong inputs", {

    exData <- data.frame(a = rep(1,10),
                         b = rep(1,10),
                         c = rep(30,10),
                         d = rep(30,10))
    rownames(exData) <- letters[5:14]

    gr1 <- factor(c("h","h","d","d"))
    gr2 <- c("h", "h", "d", "d")
    gr3 <- factor(c("h","d","h"))
    gr4 <- factor(c(NA, "h", "h", "d"))
    gr5 <- factor() # empty factor
    gr6 <- factor(c(1, 1, 2, 2)) # integers should be ok
    gr7 <- factor(c(1, 1, 1, 1))

    expect_s4_class(scudo(exData, gr1, 1, 3, 0.1), "scudoResults")

    # Tests on expressionData errors -------------------------------------------

    exData[,1] <- NA
    expect_error(scudo(exData, gr1, 1, 3, 0.1))

    exData[,1] <- letters[1:10]
    expect_error(scudo(exData, gr1, 1, 3, 0.1))

    exData <- data.frame(a = rep(1,10),
                         b = rep(1,10),
                         c = rep(30,10),
                         d = rep(30,10))

    # Test on group errors -----------------------------------------------------

    expect_error(scudo(exData, gr4, 1, 3, 0.1))

    expect_error(scudo(exData, gr2, 1, 3, 0.1))

    expect_error(scudo(exData, gr3, 1, 3, 0.1))

    expect_error(scudo(exData, gr5, 1, 3, 0.1))

    expect_s4_class(scudo(exData, gr6, 1, 3, 0.1), "scudoResults")

    expect_warning(scudo(exData, gr7, 1, 3, 0.1))

    # Test on nTop, nBottom errors ---------------------------------------------

    expect_error(scudo(exData, gr1, "a", 3, 0.1))

    expect_error(scudo(exData, gr1, 1, "a", 0.1))

    expect_error(scudo(exData, gr1, 7, 7, 0.1))

    expect_error(scudo(exData, gr1, NaN, 3, 0.1))

    expect_error(scudo(exData, gr1, 1, NaN, 0.1))

    expect_error(scudo(exData, gr1, 1, NA, 0.1))

    expect_error(scudo(exData, gr1, NA, 3, 0.1))

    expect_error(scudo(exData, gr1, -1, 3, 0.1))

    expect_error(scudo(exData, gr1, 1, -3, 0.1))

    expect_error(scudo(exData, gr1, 0.1, 3, 0.1))

    expect_error(scudo(exData, gr1, 1, 0.3, 0.1))

    expect_error(scudo(exData, gr1, Inf, 3, 0.1))

    expect_error(scudo(exData, gr1, 1, Inf, 0.1))

    expect_error(scudo(exData, gr1, integer(0), 3, 0.1))

    expect_error(scudo(exData, gr1, 1, integer(0), 0.1))

    expect_error(scudo(exData, gr1, 1, matrix(1), 0.1)) #

    expect_error(scudo(exData, gr1, 1, c(1, 2, 3), 0.1))

    expect_error(scudo(exData, gr1, 1, list(1), 0.1))

    expect_error(scudo(exData, gr1, 1, list(1, 2, 3), 0.1))

    # Test alpha ---------------------------------------------------------------

    expect_error(scudo(exData, gr1, 1, 3, "a"))

    expect_error(scudo(exData, gr1, 1, 3, NaN))

    expect_error(scudo(exData, gr1, 1, 3, -1))

    expect_error(scudo(exData, gr1, 1, 3, 0))

    expect_error(scudo(exData, gr1, 1, 3, numeric(0)))

    expect_error(scudo(exData, gr1, 1, 3, 1.2))

    expect_error(scudo(exData, gr1, 1, 3, matrix(0.5)))

    expect_error(scudo(exData, gr1, 1, 3, list(0.5)))

    expect_error(scudo(exData, gr1, 1, 3, c(0.5, 0.7)))

    # Test foldChange ----------------------------------------------------------

    expect_error(scudo(exData, gr1, 1, 3, 0.1, foldChange = logical(0)))

    expect_error(scudo(exData, gr1, 1, 3, 0.1, foldChange = "ABC"))

    expect_error(scudo(exData, gr1, 1, 3, 0.1, foldChange = c(TRUE, FALSE)))

    expect_error(scudo(exData, gr1, 1, 3, 0.1, foldChange = matrix(TRUE)))

    expect_error(scudo(exData, gr1, 1, 3, 0.1, foldChange = list(TRUE)))

    # Test groupedFoldChange ---------------------------------------------------

    expect_error(scudo(exData, gr1, 1, 3, 0.1, groupedFoldChange = logical(0)))

    expect_error(scudo(exData, gr1, 1, 3, 0.1, groupedFoldChange = "ABC"))

    expect_error(scudo(exData, gr1, 1, 3, 0.1,
        groupedFoldChange = c(TRUE, FALSE)))

    expect_error(scudo(exData, gr1, 1, 3, 0.1,
        groupedFoldChange = matrix(TRUE)))

    expect_error(scudo(exData, gr1, 1, 3, 0.1, groupedFoldChange = list(TRUE)))

    # Test featureSel ----------------------------------------------------------

    expect_error(scudo(exData, gr1, 1, 3, 0.1, featureSel = logical(0)))

    expect_error(scudo(exData, gr1, 1, 3, 0.1, featureSel =  "ABC"))

    expect_error(scudo(exData, gr1, 1, 3, 0.1, featureSel =  c(TRUE, FALSE)))

    expect_error(scudo(exData, gr1, 1, 3, 0.1, featureSel =  matrix(TRUE)))

    expect_error(scudo(exData, gr1, 1, 3, 0.1, featureSel =  list(TRUE)))

    # Test parametric ----------------------------------------------------------

    expect_error(scudo(exData, gr1, 1, 3, 0.1, parametric = logical(0)))

    expect_error(scudo(exData, gr1, 1, 3, 0.1, parametric =  "ABC"))

    expect_error(scudo(exData, gr1, 1, 3, 0.1, parametric =  c(TRUE, FALSE)))

    expect_error(scudo(exData, gr1, 1, 3, 0.1, parametric =  matrix(TRUE)))

    expect_error(scudo(exData, gr1, 1, 3, 0.1, parametric =  list(TRUE)))

    # test pAdj ----------------------------------------------------------------

    expect_error(scudo(exData, gr1, 1, 3, 0.1, pAdj = "ABC"))

    expect_error(scudo(exData, gr1, 1, 3, 0.1, pAdj = character(0)))

    expect_error(scudo(exData, gr1, 1, 10, 1))

    expect_error(scudo(exData, gr1, 1, 10, 1, pAdj = c("none", "BH")))

    # test distFun -------------------------------------------------------------

    f <- function(){}

    expect_error(scudo(exData, gr1, 1, 3, 0.1, distFun = NA))

    expect_error(scudo(exData, gr1, 1, 3, 0.1, distFun = "ABC"))

    expect_error(scudo(exData, gr1, 1, 3, 0.1, distFun = character(0)))

    expect_error(scudo(exData, gr1, 1, 3, 0.1, distFun = f))

})
