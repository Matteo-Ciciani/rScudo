context("Test utilities")
library(scudo)

test_that(".computeES works", {

    sigTop <- letters[1:3]
    sigBottom <- letters[18:20]
    exprProfile <- data.frame(rev(1:20))
    rownames(exprProfile) <- letters[1:20]

    expect_equal(.computeES(sigTop, sigBottom, exprProfile), 1)
    expect_equal(.computeES(sigBottom, sigTop, exprProfile), -1)

    sigTop <- sample(letters[1:20], 3)
    sigBottom <- sample(setdiff(letters[1:20], sigTop), 5)

    expect_lte(.computeES(sigBottom, sigTop, exprProfile), 1)
    expect_gte(.computeES(sigBottom, sigTop, exprProfile), -1)
})

test_that(".computeSignature works", {
    df <- data.frame(a = 1:10, b = rev(1:10))
    rownames(df) <- letters[11:20]
    sig <- c("t", "s", "l", "k")
    m <- matrix(c(sig, rev(sig)), ncol = 2)
    colnames(m) <- c("a", "b")

    expect_equal(apply(df, 2, .computeSignature, 2, 2), m)
})

test_that(".performScudo works", {
    exprData <- data.frame(a = 11:20,
                           b = 16:25,
                           c = rev(1:10),
                           d = c(1:2, rev(3:10)))
    rownames(exprData) <- letters[11:20]
    grps <- as.factor(c("G1", "G1", "G2", "G2"))
    nTop <- 2
    nBottom <- 3
    p <- 0.05
})
