labbcat.url <- "https://labbcat.canterbury.ac.nz/demo"

test_that("getUserInfo works", {
    if (!is.null(labbcatCredentials(labbcat.url, "demo", "demo"))) skip("Server not available")

    me <- getUserInfo(labbcat.url)
    expect_equal(me$user, "demo")
    expect_true("view" %in% me$roles)
})
