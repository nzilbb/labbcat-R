labbcat.url <- "https://labbcat.canterbury.ac.nz/demo"

test_that("getParticipantIds works", {
    if (!is.null(labbcatCredentials(labbcat.url, "demo", "demo"))) skip("Server not available")

    ids <- getParticipantIds(labbcat.url)
    expect_true(length(ids) >= 41)
    expect_true("QB247_Jacqui" %in% ids)
    expect_true("UC427_ViktoriaPapp_A_ENG" %in% ids)
})
