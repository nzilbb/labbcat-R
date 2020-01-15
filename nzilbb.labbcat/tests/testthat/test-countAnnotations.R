labbcat.url <- "https://labbcat.canterbury.ac.nz/demo"

test_that("countAnnotations works", {
    if (!is.null(labbcatCredentials(labbcat.url, "demo", "demo"))) skip("Server not available")
    
    expect_equal(countAnnotations(labbcat.url, "UC427_ViktoriaPapp_A_ENG.eaf", "orthography"), 585)
})
