labbcat.url <- "https://labbcat.canterbury.ac.nz/demo"

test_that("getMediaUrl works", {
    skip_on_cran() # don't run tests that depend on external resource on CRAN
    if (!is.null(labbcatCredentials(labbcat.url, "demo", "demo"))) skip("Server not available")

    expect_equal(
        getMediaUrl(labbcat.url, "BR2044_OllyOhlson.eaf", "_face", "video/mp4"),
        "https://labbcat.canterbury.ac.nz/demo/files/QB/BR2044_OllyOhlson/mp4/BR2044_OllyOhlson_face.mp4")
})

test_that("getMedia works", {
    skip_on_cran() # don't run tests that depend on external resource on CRAN
    if (!is.null(labbcatCredentials(labbcat.url, "demo", "demo"))) skip("Server not available")

    ## define subdirectory
    subdir <- "test-getMedia"
    
    ## download media for short interview
    media.file <- getMedia(labbcat.url, "AP2515_ErrolHitt.eaf", "", "audio/mpeg", subdir)

    expect_equal(
        media.file,
        "test-getMedia/AP2515_ErrolHitt.mp3")
    
    file.remove(media.file)
    file.remove(subdir)

})
