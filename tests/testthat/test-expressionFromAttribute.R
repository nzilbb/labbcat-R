test_that("expressionFromAttributeValue works", {
    expect_equal(
        expressionFromAttributeValue("transcript_language", c("en","en-NZ")),
        "['en','en-NZ'].includes(first('transcript_language').label)")
})
test_that("expressionFromAttributeValue works with negation", {
    expect_equal(
        expressionFromAttributeValue("transcript_language", c("en","en-NZ"), not=TRUE),
        "!['en','en-NZ'].includes(first('transcript_language').label)")
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
test_that("expressionFromAttributeValue works with a single value and negation", {
    expect_equal(
        expressionFromAttributeValue("transcript_language", "en", not=TRUE),
        "first('transcript_language').label <> 'en'")
})
test_that("expressionFromAttributeValues works", {
    expect_equal(
        expressionFromAttributeValues("participant_languages", c("en","en-NZ")),
        "['en','en-NZ'].includesAny(labels('participant_languages'))")
})
test_that("expressionFromAttributeValues works with negation", {
    expect_equal(
        expressionFromAttributeValues("participant_languages", c("en","en-NZ"), not=TRUE),
        "!['en','en-NZ'].includesAny(labels('participant_languages'))")
})
test_that("expressionFromAttributeValues works with a single value", {
    expect_equal(
        expressionFromAttributeValues("participant_languages", "en"),
        "labels('participant_languages').includes('en')")
})
test_that("expressionFromAttributeValues works with quotes", {
    expect_equal(
        expressionFromAttributeValues(
            "participant's nicknames", c("O'Mally","Dwayne \"The Rock\" Johnson")),
        "['O\\'Mally','Dwayne \"The Rock\" Johnson'].includesAny(labels('participant\\'s nicknames'))")
})
test_that("expressionFromAttributeValuesCount works", {
    expect_equal(
        expressionFromAttributeValuesCount(
            "participant_languages", count = 1),
        "labels('participant_languages').length == 1")
    expect_equal(
        expressionFromAttributeValuesCount(
            "participant_languages", ">=", 2),
        "labels('participant_languages').length >= 2")
    expect_equal(
        expressionFromAttributeValuesCount(
            "participant's nicknames", "<", 1),
        "labels('participant\\'s nicknames').length < 1")
})
