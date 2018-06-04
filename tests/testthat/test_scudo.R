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

    expect_s4_class(scudo(exData, gr1, 1, 3, 0.1), "ScudoResults")

    # Tests on expressionData errors ------------------------------------------

    exDataMa <- as.matrix(exData)

    expect_error(scudo(exDataMa, gr1, 1, 3, 0.1))

    exData[,1] <- NA
    expect_error(scudo(exData, gr1, 1, 3, 0.1))

    exData[,1] <- letters[1:10]
    expect_error(scudo(exData, gr1, 1, 3, 0.1))

    exData <- data.frame(a = rep(1,10),
                         b = rep(1,10),
                         c = rep(30,10),
                         d = rep(30,10))

    # Test on group errors ----------------------------------------------------

    expect_error(scudo(exData, gr4, 1, 3, 0.1))

    expect_error(scudo(exData, gr2, 1, 3, 0.1))

    expect_error(scudo(exData, gr3, 1, 3, 0.1))

    expect_error(scudo(exData, gr5, 1, 3, 0.1))

    expect_s4_class(scudo(exData, gr6, 1, 3, 0.1), "ScudoResults")

    expect_warning(scudo(exData, gr7, 1, 3, 0.1))

    # Test on nTop, nBottom errors --------------------------------------------

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

    # Test pValue --------------------------------------------------------------

    expect_error(scudo(exData, gr1, 1, 3, "a"))

    expect_error(scudo(exData, gr1, 1, 3, NaN))

    expect_error(scudo(exData, gr1, 1, 3, -1))

    expect_error(scudo(exData, gr1, 1, 3, 0))

    expect_error(scudo(exData, gr1, 1, 3, numeric(0)))

    expect_error(scudo(exData, gr1, 1, 3, 1.2))

    expect_error(scudo(exData, gr1, 1, 3, matrix(0.5)))

    expect_error(scudo(exData, gr1, 1, 3, list(0.5)))

    expect_error(scudo(exData, gr1, 1, 3, c(0.5, 0.7)))

    # Test prepro --------------------------------------------------------------

    expect_error(scudo(exData, gr1, 1, 3, 0.1, prepro = logical(0)))

    expect_error(scudo(exData, gr1, 1, 3, 0.1, prepro = "ABC"))

    expect_error(scudo(exData, gr1, 1, 3, 0.1, prepro = c(TRUE, FALSE)))

    expect_error(scudo(exData, gr1, 1, 3, 0.1, prepro = matrix(TRUE)))

    expect_error(scudo(exData, gr1, 1, 3, 0.1, prepro = list(TRUE)))

    # Test featureSel ----------------------------------------------------------

    expect_error(scudo(exData, gr1, 1, 3, 0.1, featureSel = logical(0)))

    expect_error(scudo(exData, gr1, 1, 3, 0.1, featureSel =  "ABC"))

    expect_error(scudo(exData, gr1, 1, 3, 0.1, featureSel =  c(TRUE, FALSE)))

    expect_error(scudo(exData, gr1, 1, 3, 0.1, featureSel =  matrix(TRUE)))

    expect_error(scudo(exData, gr1, 1, 3, 0.1, featureSel =  list(TRUE)))

    # test p.adj ---------------------------------------------------------------

    expect_error(scudo(exData, gr1, 1, 3, 0.1, p.adj = "ABC"))

    expect_error(scudo(exData, gr1, 1, 3, 0.1, p.adj = character(0)))

    expect_error(scudo(exData, gr1, 1, 10, 1))

    expect_error(scudo(exData, gr1, 1, 10, 1, p.adj = c("none", "BH")))
})

# expect_warning goes in conflict with .Normalization that raises errors:
# check commented tests

test_that("Scudo Predict selects right features.", {

    # Scudo Result for training
    exprData <- data.frame(a = 11:20,
                           b = 16:25,
                           c = rev(1:10),
                           d = c(1:2, rev(3:10)))
    rownames(exprData) <- letters[11:20]
    grps <- as.factor(c("G1", "G1", "G2", "G2"))
    nTop <- 2
    nBottom <- 3
    p <- 0.05

    res <- .performScudo(exprData, grps, nTop, nBottom, p)

    #Examples of scudo predict
    testExData <- data.frame(e = 11:20,
                           f = 16:25,
                           g = rev(1:10),
                           h = c(1:2, rev(3:10)))
    rownames(testExData) <- letters[11:20]
    grps1 <- as.factor(c("G1", "G2", "G2", "G1"))
    grps2 <- as.factor(c("G1", "G2", "G3", "G1"))

    # Testing ScudoRes obj produced
    expect_s4_class(scudoPredict(res, testExData, grps1, 1, 3), "ScudoResults")

    # tests on warnings and errors

    expect_error(scudoPredict(res, testExData, grps2, 1, 3))

    rownames(testExData) <- letters[1:10]
    expect_warning(scudoPredict(res, testExData, grps1, 1, 3))

})


