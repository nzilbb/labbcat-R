labbcat.url <- "https://labbcat.canterbury.ac.nz/demo"
labbcatCredentials(labbcat.url, "demo", "demo")

test_that("getParticipantIds works", {
    ids <- getParticipantIds(labbcat.url)
    expect_true(length(ids) >= 41)
    expect_true("QB247_Jacqui" %in% ids)
    expect_true("UC427_ViktoriaPapp_A_ENG" %in% ids)
})
