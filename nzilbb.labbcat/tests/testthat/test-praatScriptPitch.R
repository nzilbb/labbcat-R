test_that("praatScriptPitch default arguments works", {    
    script <- paste(
        "\npitchfloor = 60",
        "\nvoicingthreshold = 0.5",
        "\npitchceiling = 500",
        "\nveryaccurate$ = \"no\"",
        "\nif participant_gender$ = \"M\"",
        "\n  pitchfloor = 30",
        "\nendif",
        "\nif participant_gender$ = \"M\"",
        "\n  voicingthreshold = 0.4",
        "\nendif",
        "\nif participant_gender$ = \"\"",
        "\n  pitchceiling = 250",
        "\nendif",
        "\nselect Sound 'sampleName$'",
        "\nTo Pitch (ac): 0, pitchfloor, 15, veryaccurate$, 0.03, voicingthreshold, 0.01, 0.35,",
        "0.35, pitchceiling",
        "\nnmeanPitch = Get mean: targetStart, targetEnd, \"Hertz\"",
        "\nprint 'meanPitch' 'newline$'",
        "\nRemove\n",
        sep="")
    expect_equal(praatScriptPitch(), script)
})

test_that("praatScriptIntensity with non-default arguments works", {    
    script <- paste(
        "\npitchfloor = 50",
        "\nvoicingthreshold = 0.55",
        "\npitchceiling = 600",
        "\nveryaccurate$ = \"yes\"",
        "\nif participant_sex$ = \"male\"",
        "\n  pitchfloor = 40",
        "\nendif",
        "\nif participant_sex$ = \"male\"",
        "\n  pitchceiling = 300", pitch.ceiling.male,,
        "\nendif",
        "\nselect Sound 'sampleName$'",
        "\nTo Pitch (ac): 0.01, pitchfloor, 10, veryaccurate$, 0.035, voicingthreshold, 0.4, ",
        "0.4, 0.45, pitchceiling",
        "\nminPitch = Get minimum: targetStart, targetEnd, \"Hertz\", \"Parabolic\"",
        "\nprint 'minPitch' 'newline$'",
        "\nmaxPitch = Get maximum: targetStart, targetEnd, \"Hertz\", \"Parabolic\"",
        "\nprint 'maxPitch' 'newline$'",
        "\nRemove\n",
        sep="")
    expect_equal(praatScriptPitch(
        get.mean = FALSE, get.minimum = TRUE, get.maximum = TRUE,
        time.step = 0.01, pitch.floor = 50, max.number.of.candidates = 10, very.accurate = T,
        silence.threshold = 0.035, voicing.threshold = 0.55, octave.cost = 0.02,
        octave.jump.cost = 0.4, voiced.unvoiced.cost = 0.45,
        pitch.ceiling = 600, pitch.floor.male = 40, voicing.threshold.male = 0.55,
        pitch.ceiling.male = 300, gender.attribute = 'participant_sex', value.for.male = "male",
        window.length = 0.02, preemphasis.from = 60),
        script)
})

