context("Test scudoCytoscape")
library(scudo)

test_that("scudoCytoscape raises errors when it should", {
    exData <- data.frame(a = rep(1,10),
                         b = rep(1,10),
                         c = rep(30,10),
                         d = rep(30,10))
    rownames(exData) <- letters[5:14]

    groups <- factor(c("h","h","d","d"))
    res <- scudoTrain(exData, groups, 1, 3, 0.1)
    expect_s4_class(res, "scudoResults")

    scudoNet <- scudoNetwork(res, 0.2)
    expect_s3_class(scudoNet, "igraph")

})
