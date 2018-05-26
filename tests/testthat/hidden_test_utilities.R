context("Test utilities")
library(scudo)
source("utilities.R")

test_that(".computeES works", {

    sigTop <- letters[1:3]
    sigBottom <- letters[18:20]
    exprProfile <- rev(1:20)
    names(exprProfile) <- letters[1:20]

    expect_equal(.computeES(sigTop, sigBottom, exprProfile), -1)
    expect_equal(.computeES(sigBottom, sigTop, exprProfile), 1)

    sigTop <- sample(letters[1:20], 3)
    sigBottom <- sample(setdiff(letters[1:20], sigTop), 5)

    expect_lte(.computeES(sigBottom, sigTop, exprProfile), 1)
    expect_gte(.computeES(sigBottom, sigTop, exprProfile), -1)
})
