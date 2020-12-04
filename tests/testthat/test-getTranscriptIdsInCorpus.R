labbcat.url <- "https://labbcat.canterbury.ac.nz/demo"

test_that("getTranscriptIdsInCorpus works", {
    skip_on_cran() # don't run tests that depend on external resource on CRAN
    if (!is.null(labbcatCredentials(labbcat.url, "demo", "demo"))) skip("Server not available")

    ids <- getTranscriptIdsInCorpus(labbcat.url, "QB")
    expect_equal(length(ids), 22)
    expect_true("QB247_Jacqui.eaf" %in% ids)
    expect_false("UC427_ViktoriaPapp_A_ENG.eaf" %in% ids)
})
