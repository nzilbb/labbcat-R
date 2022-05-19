test_that("praatScriptIntensity default arguments works", {    
    script <- paste(
        "\nsubtractmean$ = \"yes\"", 
        "\nselect Sound 'sampleName$'",
        "\nTo Intensity: 100, 0, subtractmean$",
        "\nmaxIntensity = Get maximum: targetStart, targetEnd, \"Parabolic\"",
        "\nprint 'maxIntensity' 'newline$'",
        "\nRemove\n",
        sep="")
    expect_equal(praatScriptIntensity(), script)
})

test_that("praatScriptIntensity works with sample.points", {    
    script <- paste(
        "\nsubtractmean$ = \"yes\"", 
        "\nselect Sound 'sampleName$'",
        "\nTo Intensity: 100, 0, subtractmean$",
        "\npointoffset = targetAbsoluteStart + 0.3 * targetDuration",
        "\ntime_0_3_for_intensity = pointoffset",
        "\nprint 'time_0_3_for_intensity' 'newline$'",
        "\npointoffset = targetStart + 0.3 * targetDuration",
        "\nintensity_time_0_3 = Get value at time: pointoffset, \"cubic\"",
        "\nprint 'intensity_time_0_3:0' 'newline$'",
        "\npointoffset = targetAbsoluteStart + 0.7 * targetDuration",
        "\ntime_0_7_for_intensity = pointoffset",
        "\nprint 'time_0_7_for_intensity' 'newline$'",
        "\npointoffset = targetStart + 0.7 * targetDuration",
        "\nintensity_time_0_7 = Get value at time: pointoffset, \"cubic\"",
        "\nprint 'intensity_time_0_7:0' 'newline$'",
        "\nRemove\n",
        sep="")
    expect_equal(praatScriptIntensity(get.maximum = FALSE, sample.points = c(0.3, 0.7)), script)
})

test_that("praatScriptIntensity with non-default arguments works", {    
    script <- paste(
        "\nsubtractmean$ = \"no\"", 
        "\nselect Sound 'sampleName$'",
        "\nTo Intensity: 90, 0.01, subtractmean$",
        "\nmaxIntensity = Get maximum: targetStart, targetEnd, \"Parabolic\"",
        "\nprint 'maxIntensity' 'newline$'",
        "\npointoffset = targetAbsoluteStart + 0.5 * targetDuration",
        "\ntime_0_5_for_intensity = pointoffset",
        "\nprint 'time_0_5_for_intensity' 'newline$'",
        "\npointoffset = targetStart + 0.5 * targetDuration",
        "\nintensity_time_0_5 = Get value at time: pointoffset, \"linear\"",
        "\nprint 'intensity_time_0_5:0' 'newline$'",
        "\nRemove\n",
        sep="")
    expect_equal(praatScriptIntensity(minimum.pitch = 90.0, time.step = 0.01, subtract.mean = F,
                                      sample.points = c(0.5), interpolation = "linear"),
                 script)
})
