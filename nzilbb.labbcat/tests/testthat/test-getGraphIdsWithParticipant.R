labbcat.url <- "https://labbcat.canterbury.ac.nz/demo"

test_that("getGraphIdsWithParticipant works", {
    if (!is.null(labbcatCredentials(labbcat.url, "demo", "demo"))) skip("Server not available")

    ids <- getGraphIdsWithParticipant(labbcat.url, "UC427_ViktoriaPapp_A_ENG")
    expect_equal(length(ids), 1)
    expect_false("QB247_Jacqui.eaf" %in% ids)
    expect_true("UC427_ViktoriaPapp_A_ENG.eaf" %in% ids)
})
