labbcat.url <- "https://labbcat.canterbury.ac.nz/demo"
labbcatCredentials(labbcat.url, "demo", "demo")

test_that("countAnnotations works", {
    expect_equal(countAnnotations(labbcat.url, "UC427_ViktoriaPapp_A_ENG.eaf", "orthography"), 585)
})
