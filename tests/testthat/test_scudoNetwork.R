context("Test scudoNetwork")
library(scudo)

test_that("scudoNetwork throws errors when it should", {
    exData <- data.frame(a = rep(1,10),
                         b = rep(1,10),
                         c = rep(30,10),
                         d = rep(30,10))
    rownames(exData) <- letters[5:14]

    groups <- factor(c("h","h","d","d"))
    res <- scudo(exData, groups, 1, 3, 0.1)
    expect_s4_class(res, "scudoResults")

    expect_s3_class(scudoNetwork(res, 0.2), "igraph")
    expect_s3_class(scudoNetwork(res, 0.2, colors = c("#11111111", "#111111",
        "#222222", "#222222")), "igraph")

    # errors in N --------------------------------------------------------------

    expect_error(scudoNetwork(res, 1.2, colors = c("#11111111", "#111111",
        "#222222", "#222222")))

    expect_error(scudoNetwork(res, -3, colors = c("#11111111", "#111111",
        "#222222", "#222222")))

    expect_error(scudoNetwork(res, 0, colors = c("#11111111", "#111111",
        "#222222", "#222222")))

    expect_error(scudoNetwork(res, "0.2", colors = c("#11111111", "#111111",
        "#222222", "#222222")))

    expect_error(scudoNetwork(res, matrix("0.2"), colors = c("#11111111",
        "#111111", "#222222", "#222222")))

    expect_error(scudoNetwork(res, list(0.2), colors = c("#11111111", "#111111",
        "#222222", "#222222")))

    expect_error(scudoNetwork(res, numeric(0), colors = c("#11111111",
        "#111111", "#222222", "#222222")))

    expect_error(scudoNetwork(res, Inf, colors = c("#11111111", "#111111",
        "#222222", "#222222")))

    expect_error(scudoNetwork(res, NA_real_, colors = c("#11111111", "#111111",
        "#222222", "#222222")))

    expect_error(scudoNetwork(res, NaN, colors = c("#11111111", "#111111",
        "#222222", "#222222")))

    # errors in colors ---------------------------------------------------------

    expect_error(scudoNetwork(res, 0.2, colors = c("#11111111", "#111111",
        "#222222")))

    expect_error(scudoNetwork(res, 0.2, colors = c("#11111111", "#111111",
        "#123456", "#222222", "#222222")))

    expect_error(scudoNetwork(res, 0.2, colors = c("11111111", "#111111",
        "#222222", "#222222")))

    expect_error(scudoNetwork(res, 0.2, colors = c(NA, "#111111", "#222222",
        "#222222")))

    expect_error(scudoNetwork(res, 0.2, colors = NULL))

    expect_error(scudoNetwork(res, 0.2, colors = 1))

})
