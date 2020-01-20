labbcat.url <- "https://labbcat.canterbury.ac.nz/demo"

test_that("getId works", {    
    if (!is.null(labbcatCredentials(labbcat.url, "demo", "demo"))) skip("Server not available")

    expect_equal(getId(labbcat.url), labbcat.url)
})
