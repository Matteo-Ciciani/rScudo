context("Test scudo")
library(scudo)

test_that("Raises errors on wrong inputs", {

    exData <- data.frame(a = 11:20,
                          b = 16:25,
                          c = rev(1:10),
                          d = c(1:2, rev(3:10)))
    rownames(exData) <- letters[5:14]

    gr1 <- factor(c("h","h","d","d"))
    gr2 <- c("h", "h", "d", "d")
    gr3 <- factor(c("h","d","h"))
    gr4 <- factor(c(NA, "h", "h", "d"))

    # Tests on expressionData errors ------------------------------------------

    exDataMa <- as.matrix(exData)
    expect_error(scudo(exDataMa, gr1, 1, 3, 0.1))

    exData[,1] <- NA
    expect_error(scudo(exData, gr1, 1, 3, 0.1))

    exData[,1] <- letters[1:10]
    expect_error(scudo(exData, gr1, 1, 3, 0.1))

    exData <- data.frame(a = 11:20,
                         b = 16:25,
                         c = rev(1:10),
                         d = c(1:2, rev(3:10)))

    # Test on group errors ----------------------------------------------------

    expect_error(scudo(exData, gr4, 1, 3, 0.1))

    expect_error(scudo(exData, gr2, 1, 3, 0.1))

    expect_error(scudo(exData, gr3, 1, 3, 0.1))

    #expect_warning(scudo(exData, gr4, 1, 3, 0.1))

    # Test on nTop, nBottom errors --------------------------------------------

    expect_error(scudo(exData, gr1, "a", 3, 0.1))

    expect_error(scudo(exData, gr1, 1, "a", 0.1))

    expect_error(scudo(exData, gr1, NaN, 3, 0.1))

    expect_error(scudo(exData, gr1, 1, NaN, 0.1))

    expect_error(scudo(exData, gr1, 1, NA, 0.1))

    expect_error(scudo(exData, gr1, 1, 3, 0))

    #expect_warning(scudo(exData, gr1, 0, 0, 0.1))

    expect_error(scudo(exData, gr1, -1, 3, 0.1))

    expect_error(scudo(exData, gr1, 0.1, 3, 0.1))

    # Test pValue, prepro, featureSel and p.adj -------------------------------

    expect_error(scudo(exData, gr1, 1, 3, "a"))

    expect_error(scudo(exData, gr1, 1, 3, NaN))

    expect_error(scudo(exData, gr1, 1, 3, -1))

    expect_error(scudo(exData, gr1, 1, 3, 0))

    expect_error(scudo(exData, gr1, 1, 3, numeric(0)))

    expect_error(scudo(exData, gr1, 1, 3, 1.2))

    expect_error(scudo(exData, gr1, 1, 3, 0.1, prepro = logic(0)))

    expect_error(scudo(exData, gr1, 1, 3, 0.1, prepro = "ABC"))

    expect_error(scudo(exData, gr1, 1, 3, 0.1, featureSel = "ABC"))

    expect_error(scudo(exData, gr1, 1, 3, 0.1, p.adj = "ABC"))

    expect_error(scudo(exData, gr1, 1, 3, 0.1, p.adj = character(0)))

    expect_error(scudo(exData, gr1, 1, 10, 1))
})

# expect_warning goes in conflict with .Normalization that raises errors:
# check commented tests
