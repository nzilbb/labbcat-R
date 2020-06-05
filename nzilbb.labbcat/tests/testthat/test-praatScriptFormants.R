test_that("praatScriptFormants default arguments works", {    
    script <- paste(
        "\nmaxformant = 5500", 
        "\nif participant_gender$ = \"M\"",
        "\n  maxformant = 5000",
        "\nendif",
        "\nselect Sound 'sampleName$'",
        "\nTo Formant (burg): 0, 5, maxformant, 0.025, 50",
        "\npointoffset = targetAbsoluteStart + 0.5 * targetDuration",
        "\ntime_0_5 = pointoffset",
        "\nprint 'time_0_5' 'newline$'",
        "\npointoffset = targetStart + 0.5 * targetDuration",
        "\nf1_time_0_5 = Get value at time: 1, pointoffset, \"hertz\", \"Linear\"",
        "\nprint 'f1_time_0_5:0' 'newline$'",
        "\nf2_time_0_5 = Get value at time: 2, pointoffset, \"hertz\", \"Linear\"",
        "\nprint 'f2_time_0_5:0' 'newline$'",
        "\nRemove\n",
        sep="")
    expect_equal(praatScriptFormants(), script)
})

test_that("praatScriptFormants with more formants works", {    
    script <- paste(
        "\nmaxformant = 5500", 
        "\nif participant_gender$ = \"M\"",
        "\n  maxformant = 5000",
        "\nendif",
        "\nselect Sound 'sampleName$'",
        "\nTo Formant (burg): 0, 5, maxformant, 0.025, 50",
        "\npointoffset = targetAbsoluteStart + 0.5 * targetDuration",
        "\ntime_0_5 = pointoffset",
        "\nprint 'time_0_5' 'newline$'",
        "\npointoffset = targetStart + 0.5 * targetDuration",
        "\nf2_time_0_5 = Get value at time: 2, pointoffset, \"hertz\", \"Linear\"",
        "\nprint 'f2_time_0_5:0' 'newline$'",
        "\nf3_time_0_5 = Get value at time: 3, pointoffset, \"hertz\", \"Linear\"",
        "\nprint 'f3_time_0_5:0' 'newline$'",
        "\nf4_time_0_5 = Get value at time: 4, pointoffset, \"hertz\", \"Linear\"",
        "\nprint 'f4_time_0_5:0' 'newline$'",
        "\nRemove\n",
        sep="")
    expect_equal(praatScriptFormants(formants = c(2,3,4)), script)
})

test_that("praatScriptFormants with more points works", {    
    script <- paste(
        "\nmaxformant = 5500", 
        "\nif participant_gender$ = \"M\"",
        "\n  maxformant = 5000",
        "\nendif",
        "\nselect Sound 'sampleName$'",
        "\nTo Formant (burg): 0, 5, maxformant, 0.025, 50",
        
        "\npointoffset = targetAbsoluteStart + 0.25 * targetDuration",        
        "\ntime_0_25 = pointoffset",
        "\nprint 'time_0_25' 'newline$'",
        "\npointoffset = targetStart + 0.25 * targetDuration",
        "\nf1_time_0_25 = Get value at time: 1, pointoffset, \"hertz\", \"Linear\"",
        "\nprint 'f1_time_0_25:0' 'newline$'",
        "\nf2_time_0_25 = Get value at time: 2, pointoffset, \"hertz\", \"Linear\"",
        "\nprint 'f2_time_0_25:0' 'newline$'",
        
        "\npointoffset = targetAbsoluteStart + 0.5 * targetDuration",        
        "\ntime_0_5 = pointoffset",
        "\nprint 'time_0_5' 'newline$'",
        "\npointoffset = targetStart + 0.5 * targetDuration",
        "\nf1_time_0_5 = Get value at time: 1, pointoffset, \"hertz\", \"Linear\"",
        "\nprint 'f1_time_0_5:0' 'newline$'",
        "\nf2_time_0_5 = Get value at time: 2, pointoffset, \"hertz\", \"Linear\"",
        "\nprint 'f2_time_0_5:0' 'newline$'",
        
        "\npointoffset = targetAbsoluteStart + 0.75 * targetDuration",        
        "\ntime_0_75 = pointoffset",
        "\nprint 'time_0_75' 'newline$'",
        "\npointoffset = targetStart + 0.75 * targetDuration",
        "\nf1_time_0_75 = Get value at time: 1, pointoffset, \"hertz\", \"Linear\"",
        "\nprint 'f1_time_0_75:0' 'newline$'",
        "\nf2_time_0_75 = Get value at time: 2, pointoffset, \"hertz\", \"Linear\"",
        "\nprint 'f2_time_0_75:0' 'newline$'",
        
        "\nRemove\n",
        sep="")
    expect_equal(praatScriptFormants(sample.points = c(0.25, 0.5, 0.75)), script)
})

test_that("praatScriptFormants with non-default arguments works", {    
    script <- paste(
        "\nmaxformant = 6000", 
        "\nselect Sound 'sampleName$'",
        "\nTo Formant (burg): 0.01, 6, maxformant, 0.05, 60",
        "\npointoffset = targetAbsoluteStart + 0.5 * targetDuration",
        "\ntime_0_5 = pointoffset",
        "\nprint 'time_0_5' 'newline$'",
        "\npointoffset = targetStart + 0.5 * targetDuration",
        "\nf1_time_0_5 = Get value at time: 1, pointoffset, \"hertz\", \"Linear\"",
        "\nprint 'f1_time_0_5:0' 'newline$'",
        "\nf2_time_0_5 = Get value at time: 2, pointoffset, \"hertz\", \"Linear\"",
        "\nprint 'f2_time_0_5:0' 'newline$'",
        "\nRemove\n",
        sep="")
    expect_equal(praatScriptFormants(time.step = 0.01, max.number.formants = 6, max.formant = 6000, max.formant.male = 6000, gender.attribute = NA, value.for.male = NA, window.length = 0.05, preemphasis.from = 60), script)
})

