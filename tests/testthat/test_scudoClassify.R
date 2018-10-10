context("Test scudoClassify")
library(scudo)

test_that("scudoClassify raises correct errors", {

    trainExpData <- data.frame(a = rep(1,10),
                               b = rep(1,10),
                               c = rep(30,10),
                               d = rep(30,10))

    testExpData <- data.frame(e = rep(3,10),
                              f = rep(3,10),
                              g = rep(31,10),
                              h = rep(31,10))

    rownames(trainExpData) <- rownames(testExpData)  <- letters[5:14]

    N <- 0.2
    nTop <- 1
    nBottom <- 3

    gr1 <- factor(c("h","h","d","d"))
    gr2 <- c("h", "h", "d", "d")
    gr3 <- factor(c("h","d","h"))
    gr4 <- factor(c(NA, "h", "h", "d"))
    gr5 <- factor() # empty factor
    gr6 <- factor(c(1, 1, 2, 2)) # integers should be ok
    gr7 <- factor(c(1, 1, 1, 1))

    # Tests on trainExpData errors, testExpData --------------------------------

    trainExpData[,1] <- NA
    expect_error(scudoClassify(trainExpData, testExpData,
                               N, nTop, nBottom, gr1))

    trainExpData[,1] <- letters[1:10]
    expect_error(scudoClassify(trainExpData, testExpData,
                               N, nTop, nBottom, gr1))

    trainExpData <- data.frame(a = rep(1,10),
                               b = rep(1,10),
                               c = rep(30,10),
                               d = rep(30,10))

    testExpData[,1] <- NA
    expect_error(scudoClassify(trainExpData, testExpData,
                               N, nTop, nBottom, gr1))

    testExpData[,1] <- letters[1:10]
    expect_error(scudoClassify(trainExpData, testExpData,
                               N, nTop, nBottom, gr1))

    testExpData <- data.frame(e = rep(3,10),
                              f = rep(3,10),
                              g = rep(31,10),
                              h = rep(31,10))

    # Test on group errors -----------------------------------------------------

    expect_error(scudoClassify(trainExpData, testExpData,
                               N, nTop, nBottom, gr4))

    expect_error(scudoClassify(trainExpData, testExpData,
                               N, nTop, nBottom, trainGroups = gr2))

    expect_error(scudoClassify(trainExpData, testExpData,
                               N, nTop, nBottom, gr3))

    expect_error(scudoClassify(trainExpData, testExpData,
                               N, nTop, nBottom, gr5))

    expect_warning(scudoClassify(trainExpData, testExpData,
                                 N, nTop, nBottom, gr7))

    expect_error(scudoClassify(trainExpData, testExpData,  # testGroups
                               N, nTop, nBottom, gr1, testGroups = gr4))

    expect_error(scudoClassify(trainExpData, testExpData,
                               N, nTop, nBottom, gr1, testGroups = gr2))

    expect_error(scudoClassify(trainExpData, testExpData,
                               N, nTop, nBottom, gr1, testGroups = gr3))

    expect_error(scudoClassify(trainExpData, testExpData,
                               N, nTop, nBottom, gr1, testGroups = gr5))

    # Test on nTop, nBottom errors ---------------------------------------------

    expect_error(scudoClassify(trainExpData, testExpData,
                               N, "a", nBottom, gr1))

    expect_error(scudoClassify(trainExpData, testExpData,
                               N, nTop, "a", gr1))

    expect_error(scudoClassify(trainExpData, testExpData,
                               N, 7, 7, gr1))

    expect_error(scudoClassify(trainExpData, testExpData,
                               N, NaN, nBottom, gr1))

    expect_error(scudoClassify(trainExpData, testExpData,
                               N, nTop, NaN, gr1))

    expect_error(scudoClassify(trainExpData, testExpData,
                               N, NA, nBottom, gr1))

    expect_error(scudoClassify(trainExpData, testExpData,
                               N, nTop, NA, gr1))

    expect_error(scudoClassify(trainExpData, testExpData,
                               N, -1, nBottom, gr1))

    expect_error(scudoClassify(trainExpData, testExpData,
                               N, nTop, -3, gr1))

    expect_error(scudoClassify(trainExpData, testExpData,
                               N, 0.1, nBottom, gr1))

    expect_error(scudoClassify(trainExpData, testExpData,
                               N, nTop, 0.3, gr1))

    expect_error(scudoClassify(trainExpData, testExpData,
                               N, Inf, nBottom, gr1))

    expect_error(scudoClassify(trainExpData, testExpData,
                               N, nTop, Inf, gr1))

    expect_error(scudoClassify(trainExpData, testExpData,
                               N, integer(0), nBottom, gr1))

    expect_error(scudoClassify(trainExpData, testExpData,
                               N, nTop, integer(0), gr1))

    expect_error(scudoClassify(trainExpData, testExpData,
                               N, nTop, matrix(1), gr1)) #

    expect_error(scudoClassify(trainExpData, testExpData,
                               N, nTop, c(1,2,3), gr1))

    expect_error(scudoClassify(trainExpData, testExpData,
                               N, nTop, list(1), gr1))

    # Test neighbours ----------------------------------------------------------

    expect_error(scudoClassify(trainExpData, testExpData,
                               N, nTop, nBottom, gr1, neighbors = "a"))

    expect_error(scudoClassify(trainExpData, testExpData,
                               N, nTop, nBottom, gr1, neighbors = NaN))

    expect_error(scudoClassify(trainExpData, testExpData,
                               N, nTop, nBottom, gr1, neighbors = NA))

    expect_error(scudoClassify(trainExpData, testExpData,
                               N, nTop, nBottom, gr1, neighbors = -1))

    expect_error(scudoClassify(trainExpData, testExpData,
                               N, nTop, nBottom, gr1, neighbors = Inf))

    expect_error(scudoClassify(trainExpData, testExpData,
                               N, nTop, nBottom, gr1, neighbors = integer(0)))

    expect_error(scudoClassify(trainExpData, testExpData,
                               N, nTop, nBottom, gr1, neighbors = matrix(1)))

    expect_error(scudoClassify(trainExpData, testExpData,
                               N, nTop, nBottom, gr1, neighbors = c(1,2,3)))

    expect_error(scudoClassify(trainExpData, testExpData,
                               N, nTop, nBottom, gr1, neighbors = list(1)))

    # Test N -------------------------------------------------------------------

    expect_error(scudoClassify(trainExpData, testExpData,
                               N, nTop, nBottom, gr1, N = "a"))

    expect_error(scudoClassify(trainExpData, testExpData,
                               N, nTop, nBottom, gr1, N = NaN))

    expect_error(scudoClassify(trainExpData, testExpData,
                               N, nTop, nBottom, gr1, N = -1))

    expect_error(scudoClassify(trainExpData, testExpData,
                               N, nTop, nBottom, gr1, N = 0))

    expect_error(scudoClassify(trainExpData, testExpData,
                               N, nTop, nBottom, gr1, N = numeric(0)))

    expect_error(scudoClassify(trainExpData, testExpData,
                               N, nTop, nBottom, gr1, N = 1.2))

    expect_error(scudoClassify(trainExpData, testExpData,
                               N, nTop, nBottom, gr1, N = matrix(0.5)))

    expect_error(scudoClassify(trainExpData, testExpData,
                               N, nTop, nBottom, gr1, N = list(0.5)))

    expect_error(scudoClassify(trainExpData, testExpData,
                               N, nTop, nBottom, gr1, N = c(0.5, 0.7)))

    # Test alpha ---------------------------------------------------------------

    expect_error(scudoClassify(trainExpData, testExpData,
                                N, nTop, nBottom, gr1, alpha = "a"))

    expect_error(scudoClassify(trainExpData, testExpData,
                               N, nTop, nBottom, gr1, alpha = NaN))

    expect_error(scudoClassify(trainExpData, testExpData,
                               N, nTop, nBottom, gr1, alpha = -1))

    expect_error(scudoClassify(trainExpData, testExpData,
                               N, nTop, nBottom, gr1, alpha = 0))

    expect_error(scudoClassify(trainExpData, testExpData,
                               N, nTop, nBottom, gr1, alpha = numeric(0)))

    expect_error(scudoClassify(trainExpData, testExpData,
                               N, nTop, nBottom, gr1, alpha = 1.2))

    expect_error(scudoClassify(trainExpData, testExpData,
                               N, nTop, nBottom, gr1, alpha = matrix(0.5)))

    expect_error(scudoClassify(trainExpData, testExpData,
                               N, nTop, nBottom, gr1, alpha = list(0.5)))

    expect_error(scudoClassify(trainExpData, testExpData,
                               N, nTop, nBottom, gr1, alpha = c(0.5, 0.7)))

    # Test norm ----------------------------------------------------------------

    expect_error(scudoClassify(trainExpData, testExpData,
                               N, nTop, nBottom, gr1, norm = logical(0)))

    expect_error(scudoClassify(trainExpData, testExpData,
                               N, nTop, nBottom, gr1, norm = "ABC"))

    expect_error(scudoClassify(trainExpData, testExpData,
                               N, nTop, nBottom, gr1, norm = c(TRUE, FALSE)))

    expect_error(scudoClassify(trainExpData, testExpData,
                               N, nTop, nBottom, gr1, norm = matrix(TRUE)))

    expect_error(scudoClassify(trainExpData, testExpData,
                               N, nTop, nBottom, gr1, norm = list(TRUE)))

    # Test featureSel ----------------------------------------------------------

    expect_error(scudoClassify(trainExpData, testExpData,
                               N, nTop, nBottom, gr1, featureSel = logical(0)))

    expect_error(scudoClassify(trainExpData, testExpData,
                               N, nTop, nBottom, gr1, featureSel = "ABC"))

    expect_error(scudoClassify(trainExpData, testExpData,
                               N, nTop, nBottom, gr1,
                               featureSel = c(TRUE, FALSE)))

    expect_error(scudoClassify(trainExpData, testExpData,
                               N, nTop, nBottom, gr1,
                               featureSel = matrix(TRUE)))

    expect_error(scudoClassify(trainExpData, testExpData,
                               N, nTop, nBottom, gr1, featureSel = list(TRUE)))

    # Test parametric ----------------------------------------------------------

    expect_error(scudoClassify(trainExpData, testExpData,
                               N, nTop, nBottom, gr1, parametric = logical(0)))

    expect_error(scudoClassify(trainExpData, testExpData,
                               N, nTop, nBottom, gr1, parametric = "ABC"))

    expect_error(scudoClassify(trainExpData, testExpData,
                               N, nTop, nBottom, gr1,
                               parametric = c(TRUE, FALSE)))

    expect_error(scudoClassify(trainExpData, testExpData,
                               N, nTop, nBottom, gr1,
                               parametric = matrix(TRUE)))

    expect_error(scudoClassify(trainExpData, testExpData,
                               N, nTop, nBottom, gr1, parametric = list(TRUE)))

    # test pAdj ----------------------------------------------------------------

    expect_error(scudoClassify(trainExpData, testExpData,
                               N, nTop, nBottom, gr1, pAdj = "ABC"))
    expect_error(scudoClassify(trainExpData, testExpData,
                               N, nTop, nBottom, gr1, pAdj = character(0)))
    expect_error(scudoClassify(trainExpData, testExpData,
                               N, nTop, nBottom, gr1, pAdj = 1))
    expect_error(scudoClassify(trainExpData, testExpData,
                               N, nTop, nBottom, gr1, pAdj = c("none", "BH")))

    # test distFun -------------------------------------------------------------

    f <- function(){}

    expect_error(scudo(exData, gr1, 1, 3, 0.1, distFun = NA))

    expect_error(scudo(exData, gr1, 1, 3, 0.1, distFun = "ABC"))

    expect_error(scudo(exData, gr1, 1, 3, 0.1, distFun = character(0)))

    expect_error(scudo(exData, gr1, 1, 3, 0.1, distFun = f))

    expect_error(scudoClassify(trainExpData, testExpData,
                               N, nTop, nBottom, gr1, distFun = NA))
    expect_error(scudoClassify(trainExpData, testExpData,
                               N, nTop, nBottom, gr1, distFun = "ABC"))
    expect_error(scudoClassify(trainExpData, testExpData,
                               N, nTop, nBottom, gr1, distFun = character(0)))
    expect_error(scudoClassify(trainExpData, testExpData,
                               N, nTop, nBottom, gr1, distFun = f))
})
