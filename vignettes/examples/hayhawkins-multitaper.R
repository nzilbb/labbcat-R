### Author:  Patrick Reidy
### Purpose: Demo multitaper spectral analysis for Jane Stuart-Smith.
### Date:    2018-05-23
### adapted by Rachel Smith, 2018-09-18
### adapted by Robert Fromont, 2019-03-07
###  to read wavs from LaBB-CAT and write to results csv
###
### THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
### INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
### PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
### HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF
### CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE
### OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

library(ggplot2)
library(magrittr)
library(multitaper)
library(tibble)
library(tuneR)

### The R files in the ./auxiliary subdirectory of this demo define a handful of
### S4 classes, generics, and methods that wrap functionality from the tuneR and
### multitaper packages.
### You'll need to source the R files in this order because, e.g., definitions
### in later files depend on S4 classes defined in earlier files.
source('auxiliary/Waveform.R') # For reading .wav files.
source('auxiliary/Spectrum.R') # Base methods shared by all spectrum-like objects.
source('auxiliary/Periodogram.R') # For estimating spectra using the periodogram.
source('auxiliary/DPSS.R') # Windowing functions for multitaper spectra.
source('auxiliary/Multitaper.R') # For estimating spectra using multitaper method.
source('auxiliary/area.R') # For estimating area under the curve.

process.wav <- function(wav.file) {
    ## Read the contents of waveform.
    sound <- Waveform(waveform = wav.file)
    ##sound # Plot the waveform, and print some info about it.
    
    ## Read in the a restricted interval of the source WAV
    ## file using the @from and @to arguments of the Waveform function. NB: These
    ## arguments should be atomic numerics and their units should be seconds.
    ## Read in the middle 25 ms of the WAV file:
    sound <- Waveform(waveform = wav.file, from = midpoint(sound) - 0.0125, to = midpoint(sound) + 0.0125)
    sound

    ## Remove dc offset
    sound_remdc <- samples(sound)-mean(samples(sound))
    sound@samples <- sound_remdc

    ## The [sound] object is basically just a list of different pieces of information 
    ## about audio waveform data stored in "slots".
    ## The source code Waveform.R defines all of these slots.
    ##slotNames(sound)

    ## Each of the slots has an associated accessor function:
    ##bitRate(sound) # an atomic integer
    ##sampleRate(sound) # an atomic integer
    ##N(sound) # an atomic integer
    ##startTime(sound) # an atomic double
    ##samples(sound)[1:10] # an integer vector (only showing the first 10 samples)


    ## Estimate the spectrum of sound using the multitaper method.
    ## PreEmphasize and ZeroPad are functions defined in ./auxiliary/Waveform.R
    ## These functions are "data-first", so the operations can be daisy-chained 
    ## together with the forward-pipe operator (%>%) from the magrittr package.
    ## PreEmphasize applies a first-order FIR filter characterizd by:
    ##   y[n] = x[n] - alpha*x[n-1].
    ## ZeroPad just pastes zeroes onto the end of a waveform.
    ## Multitaper requires two parameters: k, the number of tapers; nw, the time-bandwidth
    ##   parameter. k=8 and nw=4 seem to be commonly used values.
    sound_spectrum <-
        sound %>%
        ##PreEmphasize(alpha = 0.5) %>% # optional
        ##ZeroPad(lengthOut = sampleRate(sound)) %>% # again, optional
        Multitaper(k = 8, nw = 4)

    ## The show-method for a Multitaper object will generate a quick plot.
    sound_spectrum

    ## Like a Waveform object, a Multitaper object is essentially a list of slots:
    ##slotNames(sound_spectrum)

    ## The slots have associated accessor functions. tapers, k, and nw access slots
    ## that track the parameter values and data tapers used to compute the MTS.
    ##values(sound_spectrum) # a numeric vector, the values ofthe MTS on the linear (i.e., not decibel) scale.
    ##binWidth(sound_spectrum) # an atomic numeric
    ##nyquist(sound_spectrum) # an atomic numeric

    ## binWidth and nyquist are used to reconstruct the vector of frequencies at which
    ## the MTS is defined:
    ##frequencies(sound_spectrum)

    ## I suspect that you want to compute some feature or set of features from the MTS.
    ## There are a few pre-defined functions for commonly computed features, e.g.
    ## spectral moments:
    ##centroid(sound_spectrum) # linear amplitude scale, all available frequencies
    ##centroid(sound_spectrum, scale = "decibel") # decibel scale, still all available frequencies
    ##centroid(sound_spectrum, minHz = 500) # linear scale, all frequencies above 500 hz
    ##centroid(sound_spectrum, scale = "decibel", minHz = 500, maxHz = 9000) # you get the idea
    ## variance, skewness, and kurtosis also come pre-defined. These functions also 
    ## have optional arguments scale, minHz, and maxHz, although I don't demo that
    ## functionality here.
    ##variance(sound_spectrum)
    ##skewness(sound_spectrum)
    ##kurtosis(soundk_spectrum)

    minAmpL <- minAmp(sound_spectrum, scale = "dB", minHz = 550, maxHz = 3000)
    maxAmpM <- maxAmp(sound_spectrum, scale = "dB", minHz= 3000, maxHz=7000)
    maxAmpH <- maxAmp(sound_spectrum, scale = "dB", minHz=7000, maxHz=nyquist(sound_spectrum))
    ampD_MminusLMin <- maxAmpM - minAmpL
    freqM <- peakHz(sound_spectrum, minHz=3000, maxHz=7000)
    freqH <- peakHz(sound_spectrum, minHz=7000, maxHz=nyquist(sound_spectrum))
    freqMH <- peakHz(sound_spectrum, minHz=3000, maxHz=nyquist(sound_spectrum))
    CoG <- centroid(sound_spectrum, scale = "decibel", minHz = 550) #check
    var <- variance(sound_spectrum, scale = "decibel", minHz = 550) #check
    areaM <- area(sound_spectrum, minHz=3000, maxHz=7000)
    areaH <- area(sound_spectrum, minHz=7000, maxHz=nyquist(sound_spectrum))
    levelD_MminusH <- areaM-areaH
    
    return(list(
        minAmpL = minAmpL,
        maxAmpM = maxAmpM,
        maxAmpH = maxAmpH,
        ampD_MminusLMin = ampD_MminusLMin,
        freqM = freqM,
        freqH = freqH,
        freqMH = freqMH,
        CoG = CoG,
        var = var,
        areaM = areaM,
        areaH = areaH,
        levelD_MminusH = levelD_MminusH
    ))
    
    ##vals <- c(minAmpL, maxAmpM, maxAmpH, ampD_MminusLMin, freqM, freqH, freqMH, CoG, var, areaM, areaH, levelD_MminusH)
    ##names <- c('minAmpL', 'maxAmpM', 'maxAmpH', 'ampD_MminusLMin', 'freqM', 'freqH', 'freqMH', 'CoG', 'var', 'areaM', 'areaH', 'levelD_MminusH')
    ##data <- data.frame(vals)
    ##rownames(data) <- names

    ##write.csv(data, "sh3.csv")
}

## Test:
##process.wav('sh3.wav')
