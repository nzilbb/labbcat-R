# NB using a local server for these tests, as 'edit' credentials are required
labbcat.url <- Sys.getenv('TEST_ADMIN_LABBCAT_URL')
username <- Sys.getenv('TEST_ADMIN_LABBCAT_USERNAME')
password <- Sys.getenv('TEST_ADMIN_LABBCAT_PASSWORD')
test.transcript.id <- "labbcat-R.test.txt"
test.media <- "labbcat-R.test.wav"

test_that("saveMedia and deleteMedia work", {
  skip_on_cran() # don't run tests that depend on external resource on CRAN
  if (!is.null(labbcatCredentials(labbcat.url, username, password))) skip("Server not available")
  
  ## ensure transcript is not already there
  ids <- getMatchingTranscriptIds(
    labbcat.url, paste("id MATCHES '",test.transcript.id,"'",sep=""))
  expect_equal(length(ids), 0)
  
  ## get attributes for new transcript
  corpus <- getCorpusIds(labbcat.url)[1]
  transcript.type <- getLayer(labbcat.url, "transcript_type")$validLabels[[1]]
  
  ## upload transcript without media
  newTranscript(
    labbcat.url, test.transcript.id, transcript.type=transcript.type, corpus=corpus,
    episode="test")
  
  ## ensure there's no media
  media <- getAvailableMedia(labbcat.url, test.transcript.id)
  expect_equal(length(media), 0)

  ## upload media
  uploaded.media = saveMedia(labbcat.url, test.transcript.id, test.media)
  expect_equal(uploaded.media$name, test.media)
  
  ## ensure there's now media
  media <- getAvailableMedia(labbcat.url, test.transcript.id)
  expect_gt(length(media), 0)

  ## delete media
  deleteMedia(labbcat.url, test.transcript.id, test.media)
  
  ## ensure there's no longer media
  media <- getAvailableMedia(labbcat.url, test.transcript.id)
  expect_equal(length(media), 0)

  ## delete transcript
  deleteTranscript(labbcat.url, test.transcript.id)
  
})
