labbcat.url <- "https://labbcat.canterbury.ac.nz/demo"

test_that("getParticipantAttributes works", {

    if (!is.null(labbcatCredentials(labbcat.url, "demo", "demo"))) skip("Server not available")

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

