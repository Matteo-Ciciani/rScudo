context("Test scudoPredict")
library(scudo)

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
    testExpData <- data.frame(e = 11:20,
                             f = 16:25,
                             g = rev(1:10),
                             h = c(1:2, rev(3:10)))
    rownames(testExpData) <- letters[11:20]
    grps1 <- as.factor(c("G1", "G2", "G2", "G1"))
    grps2 <- as.factor(c("G1", "G2", "G3", "G1"))

    # Testing ScudoRes obj produced
    expect_s4_class(scudoPredict(res, testExpData, grps1, 1, 3), "ScudoResults")

    # tests on warnings and errors

    expect_warning(scudoPredict(res, testExpData, grps2, 1, 3))

    rownames(testExpData) <- letters[1:10]
    expect_error(suppressWarnings(scudoPredict(res, testExpData, grps1, 1, 3)))

})
