labbcat.url <- "https://labbcat.canterbury.ac.nz/demo"

test_that("getSystemAttribute works for title", {
    skip_on_cran() # don't run tests that depend on external resource on CRAN
    if (!is.null(labbcatCredentials(labbcat.url, "demo", "demo"))) skip("Server not available")

    title <- getSystemAttribute(labbcat.url, "title")
    expect_true(nchar(title) > 0)
})
