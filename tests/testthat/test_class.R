context("Class validity")
library(scudo)

test_that("Class can be instantiated", {
    sres <- ScudoResults()
    expect_s4_class(sres, "ScudoResults")
})

test_that("Validity check works", {
    m <- matrix(1, ncol = 4, nrow = 4)
    diag(m) <- 0
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

    expect_s4_class(ScudoResults(DistMatrix = m,
                                 UpSignatures = SigUp,
                                 DownSignatures = SigDown,
                                 ConsensusUpSignature = ConsUp,
                                 ConsensusDownSignature = ConsDown,
                                 SelectedFeatures = Feats,
                                 Params = Pars), "ScudoResults")

    # tests are in the same order they are performed in setValidity

    expect_error(ScudoResults(DistMatrix = m[1:3,],
                              UpSignatures = SigUp,
                              DownSignatures = SigDown,
                              ConsensusUpSignature = ConsUp,
                              ConsensusDownSignature = ConsDown,
                              SelectedFeatures = Feats,
                              Params = Pars))

    m[2] <- NA
    expect_error(ScudoResults(DistMatrix = m,
                              UpSignatures = SigUp,
                              DownSignatures = SigDown,
                              ConsensusUpSignature = ConsUp,
                              ConsensusDownSignature = ConsDown,
                              SelectedFeatures = Feats,
                              Params = Pars))

    m[2] <- NaN
    expect_error(ScudoResults(DistMatrix = m,
                              UpSignatures = SigUp,
                              DownSignatures = SigDown,
                              ConsensusUpSignature = ConsUp,
                              ConsensusDownSignature = ConsDown,
                              SelectedFeatures = Feats,
                              Params = Pars))

    m[2] <- "1"
    expect_error(ScudoResults(DistMatrix = m,
                              UpSignatures = SigUp,
                              DownSignatures = SigDown,
                              ConsensusUpSignature = ConsUp,
                              ConsensusDownSignature = ConsDown,
                              SelectedFeatures = Feats,
                              Params = Pars))

    m <- matrix(1, ncol = 4, nrow = 4)
    diag(m) <- 0
    rownames(m) <- colnames(m) <- letters[1:4]
    m[1, 2] <- m[2, 1] <- -1
    expect_error(ScudoResults(DistMatrix = m,
                              UpSignatures = SigUp,
                              DownSignatures = SigDown,
                              ConsensusUpSignature = ConsUp,
                              ConsensusDownSignature = ConsDown,
                              SelectedFeatures = Feats,
                              Params = Pars))

    m[1, 2] <- m[2, 1] <- 1
    m[2] <- 5
    expect_error(ScudoResults(DistMatrix = m,
                              UpSignatures = SigUp,
                              DownSignatures = SigDown,
                              ConsensusUpSignature = ConsUp,
                              ConsensusDownSignature = ConsDown,
                              SelectedFeatures = Feats,
                              Params = Pars))

    m[2] <- 1
    m[1] <- 1
    expect_error(ScudoResults(DistMatrix = m,
                              UpSignatures = SigUp,
                              DownSignatures = SigDown,
                              ConsensusUpSignature = ConsUp,
                              ConsensusDownSignature = ConsDown,
                              SelectedFeatures = Feats,
                              Params = Pars))

    m[1] <- 0
    rownames(m) <- colnames(m) <- NULL
    expect_error(ScudoResults(DistMatrix = m,
                              UpSignatures = SigUp,
                              DownSignatures = SigDown,
                              ConsensusUpSignature = ConsUp,
                              ConsensusDownSignature = ConsDown,
                              SelectedFeatures = Feats,
                              Params = Pars))

    rownames(m) <- letters[1:4]
    colnames(m) <- letters[5:8]
    expect_error(ScudoResults(DistMatrix = m,
                              UpSignatures = SigUp,
                              DownSignatures = SigDown,
                              ConsensusUpSignature = ConsUp,
                              ConsensusDownSignature = ConsDown,
                              SelectedFeatures = Feats,
                              Params = Pars))

    colnames(m) <- letters[1:4]
    SigUp[1,1] <- NA
    expect_error(ScudoResults(DistMatrix = m,
                              UpSignatures = SigUp,
                              DownSignatures = SigDown,
                              ConsensusUpSignature = ConsUp,
                              ConsensusDownSignature = ConsDown,
                              SelectedFeatures = Feats,
                              Params = Pars))

    SigUp[1,1] <- "a"
    expect_error(ScudoResults(DistMatrix = m,
                              UpSignatures = SigUp[,1:3],
                              DownSignatures = SigDown,
                              ConsensusUpSignature = ConsUp,
                              ConsensusDownSignature = ConsDown,
                              SelectedFeatures = Feats,
                              Params = Pars))

    colnames(SigUp) <- letters[5:8]
    expect_error(ScudoResults(DistMatrix = m,
                              UpSignatures = SigUp,
                              DownSignatures = SigDown,
                              ConsensusUpSignature = ConsUp,
                              ConsensusDownSignature = ConsDown,
                              SelectedFeatures = Feats,
                              Params = Pars))

    colnames(SigUp) <- letters[1:4]
    SigUp$a <- 1:5
    expect_error(ScudoResults(DistMatrix = m,
                              UpSignatures = SigUp,
                              DownSignatures = SigDown,
                              ConsensusUpSignature = ConsUp,
                              ConsensusDownSignature = ConsDown,
                              SelectedFeatures = Feats,
                              Params = Pars))

    SigUp$a <- letters[1:5]
    SigDown[1,1] <- NA
    expect_error(ScudoResults(DistMatrix = m,
                              UpSignatures = SigUp,
                              DownSignatures = SigDown,
                              ConsensusUpSignature = ConsUp,
                              ConsensusDownSignature = ConsDown,
                              SelectedFeatures = Feats,
                              Params = Pars))

    SigDown[1,1] <- "a"
    expect_error(ScudoResults(DistMatrix = m,
                              UpSignatures = SigUp,
                              DownSignatures = SigDown[,1:3],
                              ConsensusUpSignature = ConsUp,
                              ConsensusDownSignature = ConsDown,
                              SelectedFeatures = Feats,
                              Params = Pars))

    colnames(SigDown) <- letters[5:8]
    expect_error(ScudoResults(DistMatrix = m,
                              UpSignatures = SigUp,
                              DownSignatures = SigDown,
                              ConsensusUpSignature = ConsUp,
                              ConsensusDownSignature = ConsDown,
                              SelectedFeatures = Feats,
                              Params = Pars))

    colnames(SigDown) <- letters[1:4]
    SigDown$a <- 1:10
    expect_error(ScudoResults(DistMatrix = m,
                              UpSignatures = SigUp,
                              DownSignatures = SigDown,
                              ConsensusUpSignature = ConsUp,
                              ConsensusDownSignature = ConsDown,
                              SelectedFeatures = Feats,
                              Params = Pars))

    SigDown$a <- letters[1:10]
    ConsUp[1] <- NA
    expect_error(ScudoResults(DistMatrix = m,
                              UpSignatures = SigUp,
                              DownSignatures = SigDown,
                              ConsensusUpSignature = ConsUp,
                              ConsensusDownSignature = ConsDown,
                              SelectedFeatures = Feats,
                              Params = Pars))

    ConsUp[1] <- "a"
    expect_error(ScudoResults(DistMatrix = m,
                              UpSignatures = SigUp,
                              DownSignatures = SigDown,
                              ConsensusUpSignature = ConsUp[1:2],
                              ConsensusDownSignature = ConsDown,
                              SelectedFeatures = Feats,
                              Params = Pars))

    ConsDown[1] <- NA
    expect_error(ScudoResults(DistMatrix = m,
                              UpSignatures = SigUp,
                              DownSignatures = SigDown,
                              ConsensusUpSignature = ConsUp,
                              ConsensusDownSignature = ConsDown,
                              SelectedFeatures = Feats,
                              Params = Pars))

    ConsDown[1] <- "a"
    expect_error(ScudoResults(DistMatrix = m,
                              UpSignatures = SigUp,
                              DownSignatures = SigDown,
                              ConsensusUpSignature = ConsUp,
                              ConsensusDownSignature = ConsDown[1:2],
                              SelectedFeatures = Feats,
                              Params = Pars))

    Feats[1] <- NA
    expect_error(ScudoResults(DistMatrix = m,
                              UpSignatures = SigUp,
                              DownSignatures = SigDown,
                              ConsensusUpSignature = ConsUp,
                              ConsensusDownSignature = ConsDown,
                              SelectedFeatures = Feats,
                              Params = Pars))

    # tests for Params
})
