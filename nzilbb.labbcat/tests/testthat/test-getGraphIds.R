labbcat.url <- "https://labbcat.canterbury.ac.nz/demo"

test_that("getGraphIds works", {
    if (!is.null(labbcatCredentials(labbcat.url, "demo", "demo"))) skip("Server not available")

    ids <- getGraphIds(labbcat.url)
    expect_true(length(ids) >= 28)
    expect_true("QB247_Jacqui.eaf" %in% ids)
    expect_true("UC427_ViktoriaPapp_A_ENG.eaf" %in% ids)
})
