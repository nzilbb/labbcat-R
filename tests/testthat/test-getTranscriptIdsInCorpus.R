labbcat.url <- Sys.getenv('TEST_READ_LABBCAT_URL')
username <- Sys.getenv('TEST_READ_LABBCAT_USERNAME')
password <- Sys.getenv('TEST_READ_LABBCAT_PASSWORD')

test_that("getTranscriptIdsInCorpus works", {
    skip_on_cran() # don't run tests that depend on external resource on CRAN
    if (!is.null(labbcatCredentials(labbcat.url, username, password))) skip("Server not available")

    ids <- getTranscriptIdsInCorpus(labbcat.url, "QB")
    expect_equal(length(ids), 22)
    expect_true("QB247_Jacqui.eaf" %in% ids)
    expect_false("UC427_ViktoriaPapp_A_ENG.eaf" %in% ids)
})

test_that("getTranscriptIdsInCorpus empty result is correct type", {
    skip_on_cran() # don't run tests that depend on external resource on CRAN
    if (!is.null(labbcatCredentials(labbcat.url, username, password))) skip("Server not available")

    ids <- getTranscriptIdsInCorpus(labbcat.url, "nonexistent")
    expect_equal(length(ids), 0)
})
