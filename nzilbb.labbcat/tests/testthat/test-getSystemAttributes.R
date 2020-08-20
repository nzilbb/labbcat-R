labbcat.url <- "https://labbcat.canterbury.ac.nz/demo"

test_that("getSystemAttribute works for title", {
    if (!is.null(labbcatCredentials(labbcat.url, "demo", "demo"))) skip("Server not available")

    title <- getSystemAttribute(labbcat.url, "title")
    
    expect_false(nchar(title) > 0)
})
