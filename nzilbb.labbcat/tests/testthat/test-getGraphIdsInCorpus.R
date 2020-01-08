labbcat.url <- "https://labbcat.canterbury.ac.nz/demo"
labbcatCredentials(labbcat.url, "demo", "demo")

test_that("getGraphIdsInCorpus works", {
    ids <- getGraphIdsInCorpus(labbcat.url, "QB")
    expect_equal(length(ids), 22)
    expect_true("QB247_Jacqui.eaf" %in% ids)
    expect_false("UC427_ViktoriaPapp_A_ENG.eaf" %in% ids)
})
