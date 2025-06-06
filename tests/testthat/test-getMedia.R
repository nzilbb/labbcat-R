labbcat.url <- Sys.getenv('TEST_READ_LABBCAT_URL')
username <- Sys.getenv('TEST_READ_LABBCAT_USERNAME')
password <- Sys.getenv('TEST_READ_LABBCAT_PASSWORD')

test_that("getMediaUrl works", {
    skip_on_cran() # don't run tests that depend on external resource on CRAN
    if (!is.null(labbcatCredentials(labbcat.url, username, password))) skip("Server not available")

    expect_equal(
        getMediaUrl(labbcat.url, "BR2044_OllyOhlson-b.eaf", "", "video/mp4"),
        "https://labbcat.canterbury.ac.nz/demo/files/QB/BR2044_OllyOhlson/mp4/BR2044_OllyOhlson-b.mp4")
})

test_that("getMedia works", {
    skip_on_cran() # don't run tests that depend on external resource on CRAN
    if (!is.null(labbcatCredentials(labbcat.url, username, password))) skip("Server not available")

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
