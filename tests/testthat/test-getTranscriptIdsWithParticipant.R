labbcat.url <- Sys.getenv('TEST_READ_LABBCAT_URL')
username <- Sys.getenv('TEST_READ_LABBCAT_USERNAME')
password <- Sys.getenv('TEST_READ_LABBCAT_PASSWORD')

test_that("getTranscriptIdsWithParticipant works", {
    skip_on_cran() # don't run tests that depend on external resource on CRAN
    if (!is.null(labbcatCredentials(labbcat.url, username, password))) skip("Server not available")

    ids <- getTranscriptIdsWithParticipant(labbcat.url, "UC427_ViktoriaPapp_A_ENG")
    expect_equal(length(ids), 1)
    expect_false("QB247_Jacqui.eaf" %in% ids)
    expect_true("UC427_ViktoriaPapp_A_ENG.eaf" %in% ids)
})

test_that("getTranscriptIdsWithParticipant empty result is correct type", {
    skip_on_cran() # don't run tests that depend on external resource on CRAN
    if (!is.null(labbcatCredentials(labbcat.url, username, password))) skip("Server not available")

    ids <- getTranscriptIdsWithParticipant(labbcat.url, "nonexistent")
    expect_equal(length(ids), 0)
})
