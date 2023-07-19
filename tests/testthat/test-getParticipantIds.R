labbcat.url <- Sys.getenv('TEST_READ_LABBCAT_URL')
username <- Sys.getenv('TEST_READ_LABBCAT_USERNAME')
password <- Sys.getenv('TEST_READ_LABBCAT_PASSWORD')

test_that("getParticipantIds works", {
    skip_on_cran() # don't run tests that depend on external resource on CRAN
    if (!is.null(labbcatCredentials(labbcat.url, username, password))) skip("Server not available")

    ids <- getParticipantIds(labbcat.url)
    expect_true(length(ids) >= 41)
    expect_true("QB247_Jacqui" %in% ids)
    expect_true("UC427_ViktoriaPapp_A_ENG" %in% ids)
})
