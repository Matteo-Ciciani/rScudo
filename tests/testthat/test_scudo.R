context("Test scudo")
library(scudo)

test_that("Raises errors on wrong inputs", {

    exData <- matrix(1, ncol = 4, nrow = 4)
    rownames(exData) <- letters[1:4]
    colnames(exData) <- letters[5:8]

    gr1 <- factor(c("h","h","d","d"))
    gr2 <- c("h", "h", "d", "d")
    gr3 <- factor(c("h","d","hd","h"))
    gr4 <- factor(rep("h",4))

    # Tests on expressionData errors ------------------------------------------

    exDatadf <- as.data.frame(exData)
    expect_error(scudo(exDatadf, gr1, 1, 3, 0.1))

    exData[,1] <- NA
    expect_error(scudo(exData, gr1, 1, 3, 0.1))

    exData[,1] <- NaN
    expect_error(scudo(exData, gr1, 1, 3, 0.1))

    exData[,1] <- letters[1:4]
    expect_error(scudo(exData, gr1, 1, 3, 0.1))

    exData <- matrix(1, ncol = 4, nrow = 4)
    colnames(exData) <- letters[5:8]
    rownames(exData) <- NULL
    expect_error(scudo(exData, gr1, 1, 3, 0.1))

    rownames(exData) <- letters[1:4]
    colnames(exData) <- NULL
    expect_error(scudo(exData, gr1, 1, 3, 0.1))

    colnames(exData) <- letters[5:8]

    # Test on group errors ----------------------------------------------------

    expect_error(scudo(exData, gr2, 1, 3, 0.1))

    expect_error(scudo(exData, gr3, 1, 3, 0.1))

    #expect_warning(scudo(exData, gr4, 1, 3, 0.1))

    # Test on nTop, nBottom errors --------------------------------------------

    expect_error(scudo(exData, gr1, "a", 3, 0.1))

    expect_error(scudo(exData, gr1, 1, "a", 0.1))

    expect_error(scudo(exData, gr1, NaN, 3, 0.1))

    expect_error(scudo(exData, gr1, 1, NaN, 0.1))

    #expect_warning(scudo(exData, gr1, 0, 0, 0.1))

    expect_error(scudo(exData, gr1, -1, 3, 0.1))

    expect_error(scudo(exData, gr1, 0.1, 3, 0.1))

    # Test pValue, prepro, featureSel and p.adj -------------------------------

    expect_error(scudo(exData, gr1, 1, 3, "a"))

    expect_error(scudo(exData, gr1, 1, 3, NaN))

    expect_error(scudo(exData, gr1, 1, 3, -1))

    expect_error(scudo(exData, gr1, 1, 3, 1.2))

    expect_error(scudo(exData, gr1, 1, 3, 0.1, prepro = "ABC"))

    expect_error(scudo(exData, gr1, 1, 3, 0.1, featureSel = "ABC"))

    expect_error(scudo(exData, gr1, 1, 3, 0.1, p.adj = "ABC"))

    expect_error(scudo(exData, gr1, 1, 4, 0.1))
})

# expect_warning goes in conflict with .Normalization that raises errors:
# check commented tests
