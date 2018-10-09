context("Test scudoClassifyUtilities")
library(scudo)

test_that(".computeTestNetwork works", {
    expData <- data.frame(a = 1:10, b = 2:11, c = 10:1, d = 11:2,
                          e = c(1:4, 10:5), f = c(7:10, 6:1))
    rownames(expData) <- letters[1:10]
    g <- factor(c(1,1,1,2,2,2))
    sr <- scudo(expData, g, 4, 4, alpha = 0.5, norm = FALSE, featureSel = FALSE)
    dm <- distMatrix(sr)
    net <- scudoNetwork(sr, 0.2)

    res <- .computeTestNetwork(dm, 0.2, g[-1])

    expect_identical(names(igraph::V(net)), names(igraph::V(res)))
    expect_identical(attr(igraph::E(net), "vname"),
                     attr(igraph::E(res), "vname"))
    expect_identical(as.character(igraph::V(net)$group[-1]),
                     igraph::V(res)$group[-1])
    expect_equal(igraph::E(net)$distance, igraph::E(res)$distance)
})

test_that(".networksFromDistMatrix works", {
    expData <- data.frame(a = 1:10, b = 2:11, c = 10:1, d = 11:2,
                          e = c(1:4, 10:5), f = c(7:10, 6:1),
                          g = c(8:4, 1:3, 10, 9), h = c(6:10, 5:1),
                          i = c(5:1, 6:10))
    rownames(expData) <- letters[1:10]
    g <- factor(c(1,1,1,2,2,2,1,1,1))
    sr <- scudo(expData, g, 4, 4, alpha = 0.5, norm = FALSE, featureSel = FALSE)
    dm <- distMatrix(sr)

    sr1 <- scudoNetwork(scudo(expData[, c(6, 1:5)], g[c(6, 1:5)], 4, 4,
        alpha = 0.5, norm = FALSE, featureSel = FALSE), 0.2)
    sr2 <- scudoNetwork(scudo(expData[, c(7, 1:5)], g[c(7, 1:5)], 4, 4,
        alpha = 0.5, norm = FALSE, featureSel = FALSE), 0.2)
    sr3 <- scudoNetwork(scudo(expData[, c(8, 1:5)], g[c(8, 1:5)], 4, 4,
        alpha = 0.5, norm = FALSE, featureSel = FALSE), 0.2)
    sr4 <- scudoNetwork(scudo(expData[, c(9, 1:5)], g[c(9, 1:5)], 4, 4,
        alpha = 0.5, norm = FALSE, featureSel = FALSE), 0.2)

    nets <- .networksFromDistMatrix(dm, 0.2, g[1:5])

    expect_identical(names(igraph::V(nets[[1]])), names(igraph::V(sr1)))
    expect_identical(attr(igraph::E(nets[[1]]), "vname"),
                     attr(igraph::E(sr1), "vname"))
    expect_identical(as.character(igraph::V(nets[[1]])$group[-1]),
                     as.character(igraph::V(sr1)$group[-1]))
    expect_equal(igraph::E(nets[[1]])$distance, igraph::E(sr1)$distance)

    expect_identical(names(igraph::V(nets[[2]])), names(igraph::V(sr2)))
    expect_identical(attr(igraph::E(nets[[2]]), "vname"),
                     attr(igraph::E(sr2), "vname"))
    expect_identical(as.character(igraph::V(nets[[2]])$group[-1]),
                     as.character(igraph::V(sr2)$group[-1]))
    expect_equal(igraph::E(nets[[2]])$distance, igraph::E(sr2)$distance)

    expect_identical(names(igraph::V(nets[[3]])), names(igraph::V(sr3)))
    expect_identical(attr(igraph::E(nets[[3]]), "vname"),
                     attr(igraph::E(sr3), "vname"))
    expect_identical(as.character(igraph::V(nets[[3]])$group[-1]),
                     as.character(igraph::V(sr3)$group[-1]))
    expect_equal(igraph::E(nets[[3]])$distance, igraph::E(sr3)$distance)

    expect_identical(names(igraph::V(nets[[4]])), names(igraph::V(sr4)))
    expect_identical(attr(igraph::E(nets[[4]]), "vname"),
                     attr(igraph::E(sr4), "vname"))
    expect_identical(as.character(igraph::V(nets[[4]])$group[-1]),
                     as.character(igraph::V(sr4)$group[-1]))
    expect_equal(igraph::E(nets[[4]])$distance, igraph::E(sr4)$distance)
})

test_that(".visitEdges works", {
    expData <- data.frame(a = 1:10, b = 2:11, c = 10:1, d = 11:2,
                          e = c(1:4, 10:5), f = c(7:10, 6:1),
                          g = c(8:4, 1:3, 10, 9), h = c(6:10, 5:1),
                          i = c(5:1, 6:10))
    rownames(expData) <- letters[1:10]
    g <- factor(c(1,1,1,2,2,2,1,1,1))

    sr1 <- scudo(expData[, c(9, 1:8)], g, 4, 4, alpha = 0.5, norm = FALSE,
        featureSel = FALSE)
    net1 <- scudoNetwork(sr1, 0.2)
    scores1 <- .visitEdges(net1, 1, levels(g), TRUE, 1)
    res1 <- 1:0
    names(res1) <- levels(g)
    expect_equal(scores1, res1)

    sr2 <- scudo(expData[, c(7, 1:6, 8:9)], g, 4, 4, alpha = 0.5, norm = FALSE,
                 featureSel = FALSE)
    net2 <- scudoNetwork(sr2, 0.2)
    scores2 <- .visitEdges(net2, 1, levels(g), TRUE, 1)
    res2 <- c(NaN, NaN)
    names(res2) <- levels(g)
    expect_equal(scores2, res2)

    scores3 <- .visitEdges(net1, 2, levels(g), TRUE, 1)
    edges <- igraph::get.edge.ids(net1, c("i", "a", "i", "b", "b", "a",
        "a", "e", "e", "b"), directed = FALSE)
    w <- 2 - igraph::get.edge.attribute(net1, "distance", edges)
    res3 <- c(sum(w[1:3]), sum(w[4:5]))/sum(w)
    names(res3) <- levels(g)
    expect_equal(scores3, res3)

    scores4 <- .visitEdges(net1, 2, levels(g), FALSE, 1)
    res4 <- c(3/5, 2/5)
    names(res4) <- levels(g)
    expect_equal(scores4, res4)

    scores5 <- .visitEdges(net1, 2, levels(g), TRUE, 0.5)
    edges <- igraph::get.edge.ids(net1, c("i", "a", "i", "b", "b", "a",
        "a", "e", "e", "b"), directed = FALSE)
    w <- 2 - igraph::get.edge.attribute(net1, "distance", edges)
    w <- w * c(1, 1, 0.5, 0.5, 0.5)
    res5 <- c(sum(w[1:3]), sum(w[4:5]))/sum(w)
    names(res5) <- levels(g)
    expect_equal(scores5, res5)
})

