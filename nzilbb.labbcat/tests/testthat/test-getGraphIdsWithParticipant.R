labbcat.url <- "https://labbcat.canterbury.ac.nz/demo"
labbcatCredentials(labbcat.url, "demo", "demo")

test_that("getGraphIdsWithParticipant works", {
    ids <- getGraphIdsWithParticipant(labbcat.url, "UC427_ViktoriaPapp_A_ENG")
    expect_equal(length(ids), 1)
    expect_false("QB247_Jacqui.eaf" %in% ids)
    expect_true("UC427_ViktoriaPapp_A_ENG.eaf" %in% ids)
})
