context("Check accessors")
library(scudo)

test_that("Accessors work correctly", {
    m <- matrix(1, ncol = 4, nrow = 4)
    rownames(m) <- colnames(m) <- letters[1:4]
    SigUp <- data.frame(a = letters[1:5], b = letters[6:10], c = letters[11:15],
                        d = letters[16:20], stringsAsFactors = FALSE)
    SigDown <- data.frame(a = letters[1:10], b = letters[11:20],
                          c = letters[1:10], d = letters[11:20],
                          stringsAsFactors = FALSE)
    ConsUp <- letters[11:15]
    ConsDown <- letters[16:25]
    Feats <- letters[1:20]
    Pars <- list() # to update

    ScudoRes <- ScudoResults(DistMatrix = m,
                             UpSignatures = SigUp,
                             DownSignatures = SigDown,
                             ConsensusUpSignature = ConsUp,
                             ConsensusDownSignature = ConsDown,
                             SelectedFeatures = Feats,
                             Params = Pars)

    expect_identical(DistMatrix(ScudoRes), m)
    expect_identical(UpSignatures(ScudoRes), SigUp)
    expect_identical(DownSignatures(ScudoRes), SigDown)
    expect_identical(ConsensusUpSignature(ScudoRes), ConsUp)
    expect_identical(ConsensusDownSignature(ScudoRes), ConsDown)
    expect_identical(SelectedFeatures(ScudoRes), Feats)
    #expect_identical(Params(ScudoRes), Pars)

})
