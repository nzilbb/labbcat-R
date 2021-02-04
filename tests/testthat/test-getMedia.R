labbcat.url <- "https://labbcat.canterbury.ac.nz/demo"

test_that("getMedia works", {
    skip_on_cran() # don't run tests that depend on external resource on CRAN
    if (!is.null(labbcatCredentials(labbcat.url, "demo", "demo"))) skip("Server not available")

    expect_equal(
        getMedia(labbcat.url, "BR2044_OllyOhlson.eaf", "_face", "video/mp4"),
        "https://labbcat.canterbury.ac.nz/demo/files/QB/BR2044_OllyOhlson/mp4/BR2044_OllyOhlson_face.mp4")
})
