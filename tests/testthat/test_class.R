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
    Groups <- as.factor(c("G1", "G1", "G2", "G2"))
    ConsUp <- data.frame(G1 = letters[11:15], G2 = letters[21:25],
                         stringsAsFactors = FALSE)
    ConsDown <- data.frame(G1 = letters[16:25], G2 = letters[1:10],
                           stringsAsFactors = FALSE)
    Feats <- letters[1:20]
    Pars <- list() # to update

    expect_s4_class(ScudoResults(DistMatrix = m,
                                 UpSignatures = SigUp,
                                 DownSignatures = SigDown,
                                 Groups = Groups,
                                 ConsensusUpSignatures = ConsUp,
                                 ConsensusDownSignatures = ConsDown,
                                 SelectedFeatures = Feats,
                                 Params = Pars), "ScudoResults")

    # tests are in the same order they are performed in setValidity
    # test errors in DistMatrix ------------------------------------------------
    expect_error(ScudoResults(DistMatrix = m[1:3,],
                              UpSignatures = SigUp,
                              DownSignatures = SigDown,
                              Groups = Groups,
                              ConsensusUpSignatures = ConsUp,
                              ConsensusDownSignatures = ConsDown,
                              SelectedFeatures = Feats,
                              Params = Pars))

    m[2] <- NA
    expect_error(ScudoResults(DistMatrix = m,
                              UpSignatures = SigUp,
                              DownSignatures = SigDown,
                              Groups = Groups,
                              ConsensusUpSignatures = ConsUp,
                              ConsensusDownSignatures = ConsDown,
                              SelectedFeatures = Feats,
                              Params = Pars))

    m[2] <- NaN
    expect_error(ScudoResults(DistMatrix = m,
                              UpSignatures = SigUp,
                              DownSignatures = SigDown,
                              Groups = Groups,
                              ConsensusUpSignatures = ConsUp,
                              ConsensusDownSignatures = ConsDown,
                              SelectedFeatures = Feats,
                              Params = Pars))

    m[2] <- "1"
    expect_error(ScudoResults(DistMatrix = m,
                              UpSignatures = SigUp,
                              DownSignatures = SigDown,
                              Groups = Groups,
                              ConsensusUpSignatures = ConsUp,
                              ConsensusDownSignatures = ConsDown,
                              SelectedFeatures = Feats,
                              Params = Pars))

    m <- matrix(1, ncol = 4, nrow = 4)
    diag(m) <- 0
    rownames(m) <- colnames(m) <- letters[1:4]
    m[1, 2] <- m[2, 1] <- -1
    expect_error(ScudoResults(DistMatrix = m,
                              UpSignatures = SigUp,
                              DownSignatures = SigDown,
                              Groups = Groups,
                              ConsensusUpSignatures = ConsUp,
                              ConsensusDownSignatures = ConsDown,
                              SelectedFeatures = Feats,
                              Params = Pars))

    m[1, 2] <- m[2, 1] <- 1
    m[2] <- 5
    expect_error(ScudoResults(DistMatrix = m,
                              UpSignatures = SigUp,
                              DownSignatures = SigDown,
                              Groups = Groups,
                              ConsensusUpSignatures = ConsUp,
                              ConsensusDownSignatures = ConsDown,
                              SelectedFeatures = Feats,
                              Params = Pars))

    m[2] <- 1
    m[1] <- 1
    expect_error(ScudoResults(DistMatrix = m,
                              UpSignatures = SigUp,
                              DownSignatures = SigDown,
                              Groups = Groups,
                              ConsensusUpSignatures = ConsUp,
                              ConsensusDownSignatures = ConsDown,
                              SelectedFeatures = Feats,
                              Params = Pars))

    m[1] <- 0
    rownames(m) <- colnames(m) <- NULL
    expect_error(ScudoResults(DistMatrix = m,
                              UpSignatures = SigUp,
                              DownSignatures = SigDown,
                              Groups = Groups,
                              ConsensusUpSignatures = ConsUp,
                              ConsensusDownSignatures = ConsDown,
                              SelectedFeatures = Feats,
                              Params = Pars))

    rownames(m) <- letters[1:4]
    colnames(m) <- letters[5:8]
    expect_error(ScudoResults(DistMatrix = m,
                              UpSignatures = SigUp,
                              DownSignatures = SigDown,
                              Groups = Groups,
                              ConsensusUpSignatures = ConsUp,
                              ConsensusDownSignatures = ConsDown,
                              SelectedFeatures = Feats,
                              Params = Pars))

    colnames(m) <- letters[1:4]

    # test errors in UpSignatures ----------------------------------------------
    SigUp[1,1] <- NA
    expect_error(ScudoResults(DistMatrix = m,
                              UpSignatures = SigUp,
                              DownSignatures = SigDown,
                              Groups = Groups,
                              ConsensusUpSignatures = ConsUp,
                              ConsensusDownSignatures = ConsDown,
                              SelectedFeatures = Feats,
                              Params = Pars))

    SigUp[1,1] <- "a"
    expect_error(ScudoResults(DistMatrix = m,
                              UpSignatures = SigUp[,1:3],
                              DownSignatures = SigDown,
                              Groups = Groups,
                              ConsensusUpSignatures = ConsUp,
                              ConsensusDownSignatures = ConsDown,
                              SelectedFeatures = Feats,
                              Params = Pars))

    colnames(SigUp) <- letters[5:8]
    expect_error(ScudoResults(DistMatrix = m,
                              UpSignatures = SigUp,
                              DownSignatures = SigDown,
                              Groups = Groups,
                              ConsensusUpSignatures = ConsUp,
                              ConsensusDownSignatures = ConsDown,
                              SelectedFeatures = Feats,
                              Params = Pars))

    colnames(SigUp) <- letters[1:4]
    SigUp$a <- 1:5
    expect_error(ScudoResults(DistMatrix = m,
                              UpSignatures = SigUp,
                              DownSignatures = SigDown,
                              Groups = Groups,
                              ConsensusUpSignatures = ConsUp,
                              ConsensusDownSignatures = ConsDown,
                              SelectedFeatures = Feats,
                              Params = Pars))

    SigUp$a <- letters[1:5]

    # test errors in DownSignatures --------------------------------------------
    SigDown[1,1] <- NA
    expect_error(ScudoResults(DistMatrix = m,
                              UpSignatures = SigUp,
                              DownSignatures = SigDown,
                              Groups = Groups,
                              ConsensusUpSignatures = ConsUp,
                              ConsensusDownSignatures = ConsDown,
                              SelectedFeatures = Feats,
                              Params = Pars))

    SigDown[1,1] <- "a"
    expect_error(ScudoResults(DistMatrix = m,
                              UpSignatures = SigUp,
                              DownSignatures = SigDown[,1:3],
                              Groups = Groups,
                              ConsensusUpSignatures = ConsUp,
                              ConsensusDownSignatures = ConsDown,
                              SelectedFeatures = Feats,
                              Params = Pars))

    colnames(SigDown) <- letters[5:8]
    expect_error(ScudoResults(DistMatrix = m,
                              UpSignatures = SigUp,
                              DownSignatures = SigDown,
                              Groups = Groups,
                              ConsensusUpSignatures = ConsUp,
                              ConsensusDownSignatures = ConsDown,
                              SelectedFeatures = Feats,
                              Params = Pars))

    colnames(SigDown) <- letters[1:4]
    SigDown$a <- 1:10
    expect_error(ScudoResults(DistMatrix = m,
                              UpSignatures = SigUp,
                              DownSignatures = SigDown,
                              Groups = Groups,
                              ConsensusUpSignatures = ConsUp,
                              ConsensusDownSignatures = ConsDown,
                              SelectedFeatures = Feats,
                              Params = Pars))

    SigDown$a <- letters[1:10]

    # test errors in Groups ----------------------------------------------------
    Groups[1] <- NA
    expect_error(ScudoResults(DistMatrix = m,
                              UpSignatures = SigUp,
                              DownSignatures = SigDown,
                              Groups = Groups,
                              ConsensusUpSignatures = ConsUp,
                              ConsensusDownSignatures = ConsDown,
                              SelectedFeatures = Feats,
                              Params = Pars))

    Groups[1] <- "G1"
    expect_error(ScudoResults(DistMatrix = m,
                              UpSignatures = SigUp,
                              DownSignatures = SigDown,
                              Groups = Groups[1:3],
                              ConsensusUpSignatures = ConsUp,
                              ConsensusDownSignatures = ConsDown,
                              SelectedFeatures = Feats,
                              Params = Pars))

    # test errors in ConsensusUpSignatures -------------------------------------
    ConsUp[1, 1] <- NA
    expect_error(ScudoResults(DistMatrix = m,
                              UpSignatures = SigUp,
                              DownSignatures = SigDown,
                              Groups = Groups,
                              ConsensusUpSignatures = ConsUp,
                              ConsensusDownSignatures = ConsDown,
                              SelectedFeatures = Feats,
                              Params = Pars))

    ConsUp[1, 1] <- "a"
    expect_error(ScudoResults(DistMatrix = m,
                              UpSignatures = SigUp,
                              DownSignatures = SigDown,
                              Groups = Groups,
                              ConsensusUpSignatures = ConsUp[1:2, ],
                              ConsensusDownSignatures = ConsDown,
                              SelectedFeatures = Feats,
                              Params = Pars))

    ConsUp$G1 <- 1:5
    expect_error(ScudoResults(DistMatrix = m,
                              UpSignatures = SigUp,
                              DownSignatures = SigDown,
                              Groups = Groups,
                              ConsensusUpSignatures = ConsUp,
                              ConsensusDownSignatures = ConsDown,
                              SelectedFeatures = Feats,
                              Params = Pars))

    ConsUp$G1 = letters[11:15]
    colnames(ConsUp) <- c("G1", "G3")
    expect_error(ScudoResults(DistMatrix = m,
                              UpSignatures = SigUp,
                              DownSignatures = SigDown,
                              Groups = Groups,
                              ConsensusUpSignatures = ConsUp,
                              ConsensusDownSignatures = ConsDown,
                              SelectedFeatures = Feats,
                              Params = Pars))
    colnames(ConsUp) <- c("G1", "G2")

    # test errors in ConsensusDownSignatures -----------------------------------
    ConsDown[1, 1] <- NA
    expect_error(ScudoResults(DistMatrix = m,
                              UpSignatures = SigUp,
                              DownSignatures = SigDown,
                              Groups = Groups,
                              ConsensusUpSignatures = ConsUp,
                              ConsensusDownSignatures = ConsDown,
                              SelectedFeatures = Feats,
                              Params = Pars))

    ConsDown[1, 1] <- letters[16]
    expect_error(ScudoResults(DistMatrix = m,
                              UpSignatures = SigUp,
                              DownSignatures = SigDown,
                              Groups = Groups,
                              ConsensusUpSignatures = ConsUp,
                              ConsensusDownSignatures = ConsDown[1:2, ],
                              SelectedFeatures = Feats,
                              Params = Pars))

    ConsDown$G2 <- 1:10
    expect_error(ScudoResults(DistMatrix = m,
                              UpSignatures = SigUp,
                              DownSignatures = SigDown,
                              Groups = Groups,
                              ConsensusUpSignatures = ConsUp,
                              ConsensusDownSignatures = ConsDown,
                              SelectedFeatures = Feats,
                              Params = Pars))

    ConsDown$G2 <- letters[1:10]
    colnames(ConsDown) <- c("G1", "G3")
    expect_error(ScudoResults(DistMatrix = m,
                              UpSignatures = SigUp,
                              DownSignatures = SigDown,
                              Groups = Groups,
                              ConsensusUpSignatures = ConsUp,
                              ConsensusDownSignatures = ConsDown,
                              SelectedFeatures = Feats,
                              Params = Pars))
    colnames(ConsDown) <- c("G1", "G2")

    # test errors in Features --------------------------------------------------
    Feats[1] <- NA
    expect_error(ScudoResults(DistMatrix = m,
                              UpSignatures = SigUp,
                              DownSignatures = SigDown,
                              Groups = Groups,
                              ConsensusUpSignatures = ConsUp,
                              ConsensusDownSignatures = ConsDown,
                              SelectedFeatures = Feats,
                              Params = Pars))

    # tests for Params ---------------------------------------------------------
})
