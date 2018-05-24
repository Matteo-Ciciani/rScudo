context("Class validity")
library(scudo)

test_that("Class can be instantiated", {
    sres <- ScudoResults()
    expect_s4_class(sres, "ScudoResults")
})

test_that("Validity check works", {
    m <- matrix(1, ncol = 5, nrow = 5)
    SigUp <- data.frame(a = letters[1:5], b = letters[6:10],
                        stringsAsFactors = FALSE)
    SigDown <- data.frame(a = letters[1:10], b = letters[11:20],
                          stringsAsFactors = FALSE)
    ConsUp <- letters[11:15]
    ConsDown <- letters[16:25]
    Feats <- letters[1:20]
    Pars <- list() # to update

    expect_s4_class(ScudoResults(DistMatrix = m,
                                 UpSignatures = SigUp,
                                 DownSignatures = SigDown,
                                 ConsensusUpSignature = ConsUp,
                                 ConsensusDownSignature = ConsDown,
                                 SelectedFeatures = Feats,
                                 Params = Pars))

    # tests are in the same order they are performed in setValidity

    expect_error(ScudoResults(DistMatrix = m[1:4,],
                              UpSignatures = SigUp,
                              DownSignatures = SigDown,
                              ConsensusUpSignature = ConsUp,
                              ConsensusDownSignature = ConsDown,
                              SelectedFeatures = Feats,
                              Params = Pars))
    m[12] <- NA
    expect_error(ScudoResults(DistMatrix = m,
                                 UpSignatures = SigUp,
                                 DownSignatures = SigDown,
                                 ConsensusUpSignature = ConsUp,
                                 ConsensusDownSignature = ConsDown,
                                 SelectedFeatures = Feats,
                                 Params = Pars))
    m[12] <- NaN
    expect_error(ScudoResults(DistMatrix = m,
                              UpSignatures = SigUp,
                              DownSignatures = SigDown,
                              ConsensusUpSignature = ConsUp,
                              ConsensusDownSignature = ConsDown,
                              SelectedFeatures = Feats,
                              Params = Pars))
    m[12] <- "1"
    expect_error(ScudoResults(DistMatrix = m,
                              UpSignatures = SigUp,
                              DownSignatures = SigDown,
                              ConsensusUpSignature = ConsUp,
                              ConsensusDownSignature = ConsDown,
                              SelectedFeatures = Feats,
                              Params = Pars))
    m <- matrix(1, ncol = 5, nrow = 5)
    expect_error(ScudoResults(DistMatrix = m,
                              UpSignatures = SigUp,
                              DownSignatures = SigDown,
                              ConsensusUpSignature = ConsUp,
                              ConsensusDownSignature = ConsDown,
                              SelectedFeatures = Feats,
                              Params = Pars))
    expect_error(ScudoResults(DistMatrix = m,
                              UpSignatures = SigUp,
                              DownSignatures = SigDown,
                              ConsensusUpSignature = ConsUp,
                              ConsensusDownSignature = ConsDown,
                              SelectedFeatures = Feats,
                              Params = Pars))
    expect_error(ScudoResults(DistMatrix = m,
                              UpSignatures = SigUp,
                              DownSignatures = SigDown,
                              ConsensusUpSignature = ConsUp,
                              ConsensusDownSignature = ConsDown,
                              SelectedFeatures = Feats,
                              Params = Pars))
    expect_error(ScudoResults(DistMatrix = m,
                              UpSignatures = SigUp,
                              DownSignatures = SigDown,
                              ConsensusUpSignature = ConsUp,
                              ConsensusDownSignature = ConsDown,
                              SelectedFeatures = Feats,
                              Params = Pars))
    expect_error(ScudoResults(DistMatrix = m,
                              UpSignatures = SigUp,
                              DownSignatures = SigDown,
                              ConsensusUpSignature = ConsUp,
                              ConsensusDownSignature = ConsDown,
                              SelectedFeatures = Feats,
                              Params = Pars))
})
