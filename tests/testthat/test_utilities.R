context("Test utilities")
library(rScudo)

test_that(".computeES works", {

    sigTop <- 1:3
    sigBottom <- 18:20
    nTop <- 3
    nBottom <- 3
    exprProfile <- 1:20
    names(exprProfile) <- letters[1:20]

    expect_equal(.computeES(sigTop, sigBottom, exprProfile), 1)
    expect_equal(.computeES(sigBottom, sigTop, exprProfile), -1)

    sigTop <- sample(1:20, 3)
    sigBottom <- sample(setdiff(1:20, sigTop), 5)

    expect_lte(.computeES(sigBottom, sigTop, exprProfile), 1)
    expect_gte(.computeES(sigBottom, sigTop, exprProfile), -1)
})

test_that(".computeSignature works", {
    df <- data.frame(a = 1:10, b = rev(1:10))
    rownames(df) <- letters[11:20]
    idx <- apply(df, 2, order, decreasing = TRUE)
    rownames(idx) <- rownames(df)
    sig <- c("t", "s", "l", "k")
    m <- matrix(c(sig, rev(sig)), ncol = 2)
    colnames(m) <- c("a", "b")

    expect_equal(apply(idx, 2, .computeSignature, 2, 2), m)
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

    res <- .performScudo(exprData, grps, nTop, nBottom, NULL, p, FALSE, FALSE,
                         FALSE, FALSE, "none")
    expect_s4_class(res, "ScudoResults")

    d_ad <- 1 - ((-6/8 + 2/3)/2 + (-6/8 + 2/3)/2)/2
    d_cd <- 1 - ((-1 + 5/7)/2 + (6/8 - 2/3)/2)/2
    m <- matrix(c(0,    0,    2,    d_ad,
                  0,    0,    2,    d_ad,
                  2,    2,    0,    d_cd,
                  d_ad, d_ad, d_cd, 0   ),
                ncol = 4)
    m[m != 0] <- m[m != 0] - 1.04
    rownames(m) <- colnames(m) <- letters[1:4]
    expect_equal(distMatrix(res), m)

    up <- data.frame(a = c("t", "s"), b = c("t", "s"), c = c("k", "l"),
                     d = c("m", "n"), stringsAsFactors = FALSE)
    expect_identical(upSignatures(res), up)

    down <- data.frame(a = c("m", "l", "k"), b = c("m", "l", "k"),
        c = c("r", "s", "t"), d = c("t", "l", "k"), stringsAsFactors = FALSE)
    expect_identical(downSignatures(res), down)

    consUp <- data.frame(G1 = c("t", "s"), G2 = c("m", "n"),
                         stringsAsFactors = FALSE)
    expect_identical(consensusUpSignatures(res), consUp)

    consDown <- data.frame(G1 = c("m", "l", "k"), G2 = c("r", "s", "t"),
                           stringsAsFactors = FALSE)
    expect_identical(consensusDownSignatures(res), consDown)

    expect_identical(groupsAnnotation(res), grps)
    expect_identical(selectedFeatures(res), letters[11:20])
    expect_identical(scudoParams(res), list(nTop = nTop, nBottom = nBottom,
        alpha = p, foldChange = FALSE, groupedFoldChange = FALSE,
        featureSel = FALSE, parametric = FALSE, pAdj = "none"))
})

test_that(".computeFC works correctly", {
    df <- data.frame(a = rep(1, 5),
                     b = rep(3, 5),
                     c = rep(5, 5),
                     d = 1:5,
                     e = 11:15)
    rownames(df) <- letters[21:25]
    groups <- factor(c(1, 1, 1, 2, 2))

    virtContr <- (3 + 6:10) / 2
    correctRes <- df - virtContr

    expect_equal(.computeFC(df, groups, NULL), correctRes)
})
