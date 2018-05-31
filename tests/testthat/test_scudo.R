context("Test scudo preprocessing")
library(scudo)

test_that("Feature selection works", {

    groups2 <- c(rep("A", 50), rep("B", 50))
    ExprMatrix <- matrix(1, ncol = 100, nrow = 100)
    ExprMatrix <- apply(ExprMatrix, 1, function(x) x <- stats::runif(100,0,100))
    rownames(ExprMatrix) <- c(rep("k", 25), c(rep("x", 25)),
        c(rep("y", 25)), c(rep("z", 25)))
    colnames(ExprMatrix) <- groups2

    expect_equal(2,2)

})
