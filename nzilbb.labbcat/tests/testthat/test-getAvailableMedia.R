labbcat.url <- "https://labbcat.canterbury.ac.nz/demo"
labbcatCredentials(labbcat.url, "demo", "demo")

test_that("getAvailableMedia works", {
    media <- getAvailableMedia(labbcat.url, "BR2044_OllyOhlson.eaf")
    expect_equal(length(media$name), 3)
    expect_equal(length(media$type), 3)
    expect_equal(length(media$extension), 3)
    expect_equal(length(media$mimeType), 3)
    expect_equal(length(media$url), 3)
    expect_equal(length(media$nameWithoutSuffix), 3)
    expect_equal(length(media$trackSuffix), 3)

    expect_true("BR2044_OllyOhlson.mp4" %in% media$name)
    expect_true("BR2044_OllyOhlson_face.mp4" %in% media$name)
    expect_true("BR2044_OllyOhlson.wav" %in% media$name)
    
    expect_true("video" %in% media$type)
    expect_true("mp4" %in% media$extension)
    expect_true("video/mp4" %in% media$mimeType)
    expect_true("BR2044_OllyOhlson" %in% media$nameWithoutSuffix)
    expect_true("_face" %in% media$trackSuffix)
})
