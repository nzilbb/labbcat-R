labbcat.url <- "https://labbcat.canterbury.ac.nz/demo"
labbcatCredentials(labbcat.url, "demo", "demo")

test_that("getGraphIds works", {
    ids <- getGraphIds(labbcat.url)
    expect_true(length(ids) >= 28)
    expect_true("QB247_Jacqui.eaf" %in% ids)
    expect_true("UC427_ViktoriaPapp_A_ENG.eaf" %in% ids)
})
