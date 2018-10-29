context("Check accessors")
library(rScudo)

test_that("Accessors work correctly", {
    m <- matrix(1, ncol = 4, nrow = 4)
    diag(m) <- 0
    rownames(m) <- colnames(m) <- letters[1:4]
    SigUp <- data.frame(a = letters[1:5], b = letters[6:10], c = letters[11:15],
                        d = letters[16:20], stringsAsFactors = FALSE)
    SigDown <- data.frame(a = letters[1:10], b = letters[11:20],
                          c = letters[1:10], d = letters[11:20],
                          stringsAsFactors = FALSE)
    groups <- as.factor(c("G1", "G1", "G2", "G2"))
    ConsUp <- data.frame(G1 = letters[11:15], G2 = letters[21:25],
                         stringsAsFactors = FALSE)
    ConsDown <- data.frame(G1 = letters[16:25], G2 = letters[1:10],
                           stringsAsFactors = FALSE)
    Feats <- letters[1:20]
    Pars <- list() # to update

    ScudoRes <- ScudoResults(distMatrix = m,
                             upSignatures = SigUp,
                             downSignatures = SigDown,
                             groupsAnnotation = groups,
                             consensusUpSignatures = ConsUp,
                             consensusDownSignatures = ConsDown,
                             selectedFeatures = Feats,
                             scudoParams = Pars)

    expect_identical(distMatrix(ScudoRes), m)
    expect_identical(upSignatures(ScudoRes), SigUp)
    expect_identical(downSignatures(ScudoRes), SigDown)
    expect_identical(groupsAnnotation(ScudoRes), groups)
    expect_identical(consensusUpSignatures(ScudoRes), ConsUp)
    expect_identical(consensusDownSignatures(ScudoRes), ConsDown)
    expect_identical(selectedFeatures(ScudoRes), Feats)
    #expect_identical(scudoParams(ScudoRes), Pars)

})
