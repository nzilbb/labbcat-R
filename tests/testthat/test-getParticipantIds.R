labbcat.url <- "https://labbcat.canterbury.ac.nz/demo"

test_that("getParticipantIds works", {
    skip_on_cran() # don't run tests that depend on external resource on CRAN
    if (!is.null(labbcatCredentials(labbcat.url, "demo", "demo"))) skip("Server not available")

    ids <- getParticipantIds(labbcat.url)
    expect_true(length(ids) >= 41)
    expect_true("QB247_Jacqui" %in% ids)
    expect_true("UC427_ViktoriaPapp_A_ENG" %in% ids)
})
