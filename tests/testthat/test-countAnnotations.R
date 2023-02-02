labbcat.url <- "https://labbcat.canterbury.ac.nz/demo"

test_that("countAnnotations works", {
    skip_on_cran() # don't run tests that depend on external resource on CRAN
    if (!is.null(labbcatCredentials(labbcat.url, "demo", "demo"))) skip("Server not available")
    
    expect_equal(countAnnotations(labbcat.url, "UC427_ViktoriaPapp_A_ENG.eaf", "orthography"), 585)
})

test_that("countAnnotations max.ordinal parameter works", {
    skip_on_cran() # don't run tests that depend on external resource on CRAN
    if (!is.null(labbcatCredentials(labbcat.url, "demo", "demo"))) skip("Server not available")

    all.annotations <- countAnnotations(
        labbcat.url, "UC427_ViktoriaPapp_A_ENG.eaf", "phonemes")
    first.annotations <- countAnnotations(
        labbcat.url, "UC427_ViktoriaPapp_A_ENG.eaf", "phonemes", max.ordinal = 1)
    
    expect_true(first.annotations < all.annotations)
})
