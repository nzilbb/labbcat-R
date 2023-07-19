labbcat.url <- Sys.getenv('TEST_READ_LABBCAT_URL')
username <- Sys.getenv('TEST_READ_LABBCAT_USERNAME')
password <- Sys.getenv('TEST_READ_LABBCAT_PASSWORD')

test_that("getParticipantAttributes works", {
    skip_on_cran() # don't run tests that depend on external resource on CRAN
    if (!is.null(labbcatCredentials(labbcat.url, username, password))) skip("Server not available")

    participantIds <- getParticipantIds(labbcat.url)
    attributes <- getParticipantAttributes(
        labbcat.url,
        participantIds,
        c("participant_gender", "participant_age_category"))

    ## check dataframe columns
    expect_equal(length(attributes$participant), length(participantIds))
    expect_equal(length(attributes$participant_gender), length(participantIds))
    expect_equal(length(attributes$participant_age_category), length(participantIds))

})

