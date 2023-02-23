test_that("expressionFromAttributeValue works", {
    expect_equal(
        expressionFromAttributeValue("transcript_language", c("en","en-NZ")),
        "['en','en-NZ'].includes(first('transcript_language').label)")
})
test_that("expressionFromAttributeValue works with quotes", {
    expect_equal(
        expressionFromAttributeValue(
            "participant's nicknames", c("O'Mally","Dwayne \"The Rock\" Johnson")),
        "['O\\'Mally','Dwayne \"The Rock\" Johnson'].includes(first('participant\\'s nicknames').label)")
})
test_that("expressionFromAttributeValue works with a single value", {
    expect_equal(
        expressionFromAttributeValue("transcript_language", "en"),
        "first('transcript_language').label == 'en'")
})
test_that("expressionFromAttributeValues works", {
    expect_equal(
        expressionFromAttributeValues("participant_languages", c("en","en-NZ")),
        "['en','en-NZ'].includesAny(labels('participant_languages'))")
})
test_that("expressionFromAttributeValues works with a single value", {
    expect_equal(
        expressionFromAttributeValues("participant_languages", "en"),
        "labels('participant_languages').includes('en')")
})
test_that("expressionFromAttributeValues works works with quotes", {
    expect_equal(
        expressionFromAttributeValues(
            "participant's nicknames", c("O'Mally","Dwayne \"The Rock\" Johnson")),
        "['O\\'Mally','Dwayne \"The Rock\" Johnson'].includesAny(labels('participant\\'s nicknames'))")
})
