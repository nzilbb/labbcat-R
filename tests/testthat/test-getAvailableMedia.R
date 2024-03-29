labbcat.url <- Sys.getenv('TEST_READ_LABBCAT_URL')
username <- Sys.getenv('TEST_READ_LABBCAT_USERNAME')
password <- Sys.getenv('TEST_READ_LABBCAT_PASSWORD')

test_that("getAvailableMedia works", {
    skip_on_cran() # don't run tests that depend on external resource on CRAN
    if (!is.null(labbcatCredentials(labbcat.url, username, password))) skip("Server not available")

    media <- getAvailableMedia(labbcat.url, "BR2044_OllyOhlson.eaf")
    expect_equal(length(media$name), 9)
    expect_equal(length(media$mimeType), 9)
    expect_equal(length(media$url), 9)
    expect_equal(length(media$trackSuffix), 9)

    expect_true("BR2044_OllyOhlson.mp4" %in% media$name)
    expect_true("BR2044_OllyOhlson_face.mp4" %in% media$name)
    expect_true("BR2044_OllyOhlson.wav" %in% media$name)
    
    expect_true("video" %in% media$type)
    expect_true("mp4" %in% media$extension)
    expect_true("video/mp4" %in% media$mimeType)
    expect_true("BR2044_OllyOhlson" %in% media$nameWithoutSuffix)
    expect_true("_face" %in% media$trackSuffix)
})
