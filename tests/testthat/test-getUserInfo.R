labbcat.url <- "https://labbcat.canterbury.ac.nz/demo"

test_that("getUserInfo works", {
    skip_on_cran() # don't run tests that depend on external resource on CRAN
    if (!is.null(labbcatCredentials(labbcat.url, "demo", "demo"))) skip("Server not available")

    me <- getUserInfo(labbcat.url)
    expect_equal(me$user, "demo")
    expect_true("view" %in% me$roles)
})
