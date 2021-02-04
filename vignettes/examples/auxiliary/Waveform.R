# Author:       Patrick Reidy
# Affiliations: The Ohio State University, Department of Linguistics
# Date:         May 21, 2014
# Purpose:      Define the Waveform S4 class and methods for manipulating
#               objects of that class.
# Dependencies: ggplot2
#               tuneR
# THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
# INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
# PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
# HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF
# CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE
# OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.



# Quietly load the dependencies.
suppressPackageStartupMessages(library('ggplot2'))
suppressPackageStartupMessages(library('tuneR'))





################################################################################
# Class definition                                                             #
################################################################################

setClass(
  Class = 'Waveform',
  contains = c(),
  slots    = c(samples    = 'numeric',
               bitRate    = 'numeric',
               sampleRate = 'numeric',
               startTime  = 'numeric',
               N          = 'numeric')
  )





################################################################################
# Object construction                                         Waveform objects #
################################################################################

setGeneric(
  name = 'Waveform',
  def  = function(waveform, startTime, ...)
    standardGeneric('Waveform'))

setMethod(
  f   = 'Waveform',
  sig = c(waveform = 'Wave', startTime = 'numeric'),
  def = function(waveform, startTime) {
    # Determine the value of the @amplitude slot according to whether
    # the [waveform] argument is mono or stereo...
    if (waveform@stereo) {
      # If the Wave object is stereo, ask the user which channel they would
      # like to use.
      message("Initializing a mono Waveform object from a stereo Wave object...")
      flush.console()
      channel <- select.list(title     = "Which channel would you like to use?",
                             choices   = c("Left", "Right", "Average"),
                             preselect = "Left", graphics = TRUE)
      if (channel == "Left") {
        wave.amp <- waveform@left
      } else if (channel == "Right") {
        wave.amp <- waveform@right
      } else if (channel == "Average") {
        wave.amp <- (waveform@left + waveform@right) / 2
      } else {
        wave.amp <- as.numeric(NA)
      }
    } else {
      # If the Wave object is mono, then use the left channel.
      wave.amp <- waveform@left
    }
    # Initialize the Waveform object.
    new(Class = 'Waveform',
        samples    = wave.amp,
        bitRate    = waveform@bit,
        sampleRate = waveform@samp.rate,
        startTime  = startTime,
        N          = length(wave.amp))
  })

setMethod(
  f   = 'Waveform',
  sig = c(waveform = 'Wave', startTime = 'missing'),
  def = function(waveform)
    Waveform(waveform, startTime = 0.0))

setMethod(
  f   = 'Waveform',
  sig = c(waveform = 'character', startTime = 'numeric'),
  def = function(waveform, startTime, from = startTime, to = Inf) 
    Waveform(waveform = readWave(filename = waveform, from = from, to = to,
                                 units = 'seconds'),
             startTime = startTime))

setMethod(
  f   = 'Waveform',
  sig = c(waveform = 'character', startTime = 'missing'),
  def = function(waveform, from = 0.0, to = Inf, preserveTimes = TRUE) {
    if (preserveTimes) startTime <- from else startTime <- 0.0
    Waveform(waveform = waveform, startTime = startTime, from = from, to = to)
    })






################################################################################
# Methods                                                                      #
################################################################################

#############################################################################
#  amplitudeRange                                            amplitudeRange #
#############################################################################

# amplitudeRange
if (! isGeneric('amplitudeRange'))
  setGeneric(
    name = 'amplitudeRange',
    def  = function(x) standardGeneric('amplitudeRange'))

setMethod(
  f   = 'amplitudeRange',
  sig = c(x = 'Waveform'),
  def = function(x) max(samples(x)) - min(samples(x)))


#############################################################################
#  @bitRate get-method                                             @bitRate #
#############################################################################

# bitRate
if (! isGeneric('bitRate'))
  setGeneric(
    name = 'bitRate',
    def  = function(x) standardGeneric('bitRate'))

setMethod(
  f   = 'bitRate',
  sig = c(x = 'Waveform'),
  def = function(x) x@bitRate)


#############################################################################
#  Duration of Waveform object                                     duration #
#############################################################################

# duration
if (! isGeneric('duration'))
  setGeneric(
    name = 'duration',
    def  = function(x) standardGeneric('duration'))

setMethod(
  f   = 'duration',
  sig = c(x = 'Waveform'),
  def = function(x) endTime(x) - startTime(x))


#############################################################################
#  Time of last sample                                              endTime #
#############################################################################

# endTime
if (! isGeneric('endTime'))
  setGeneric(
    name = 'endTime',
    def  = function(x) standardGeneric('endTime'))

setMethod(
  f   = 'endTime',
  sig = c(x = 'Waveform'),
  def = function(x) times(x)[length(times(x))])


#############################################################################
#  Hamming window                                                   Hamming #
#############################################################################

# Hamming
if (! isGeneric('Hamming'))
  setGeneric(
    name = 'Hamming',
    def  = function(x, ...) standardGeneric('Hamming'))

setMethod(
  f   = 'Hamming',
  sig = c(x = 'Waveform'),
  def = function(x, alpha = 0.53836) {
    .alpha <- alpha
    .beta  <- 1 - .alpha
    .n     <- 0:(N(x) - 1)
    .hamming <- .alpha - (.beta * cos(2*pi*.n / (N(x) - 1)))
    .samps   <- samples(x)[1:N(x)]
    .samps   <- .hamming * .samps
    .pad     <- rep(0, length(x) - N(x))
    x@samples <- c(.samps, .pad)
    return(x)
  })


#############################################################################
#  Length (different from number of samples)                         length #
#############################################################################

setMethod(
  f   = 'length',
  sig = c(x = 'Waveform'),
  def = function(x) {
    length(samples(x))
  }
)


#############################################################################
#  Temporal midpoint                                               midpoint #
#############################################################################

# midpoint
if (! isGeneric('midpoint'))
  setGeneric(
    name = 'midpoint',
    def  = function(x) standardGeneric('midpoint'))

setMethod(
  f   = 'midpoint',
  sig = c(x = 'Waveform'),
  def = function(x) (startTime(x) + endTime(x)) / 2)


#############################################################################
#  @N get-method                                                         @N #
#############################################################################

# N
if (! isGeneric('N'))
  setGeneric(
    name = 'N',
    def  = function(x) standardGeneric('N'))

setMethod(
  f   = 'N',
  sig = c(x = 'Waveform'),
  def = function(x) x@N)


#############################################################################
# Pre-emphasize a waveform                                    pre-emphasize #
#############################################################################

# PreEmphasize
if (! isGeneric('PreEmphasize'))
  setGeneric(
    name = 'PreEmphasize',
    def  = function(waveform, alpha)
      standardGeneric('PreEmphasize'))

setMethod(
  f   = 'PreEmphasize',
  sig = c(waveform = 'Waveform', alpha = 'numeric'),
  def = function(waveform, alpha) {
    .samps <- samples(waveform)[1:N(waveform)]
    .samps <- .samps - (alpha * c(0, .samps[1:(N(waveform)-1)]))
    .pad   <- rep(0, times = length(waveform) - N(waveform))
    waveform@samples <- c(.samps, .pad)
    return(waveform)
  })



#############################################################################
#  Shingling rectangular windows                         RectangularWindows #
#############################################################################

# RectangularWindows
if (! isGeneric('RectangularWindows'))
  setGeneric(
    name = 'RectangularWindows',
    def  = function(waveform, from, to, windowDur, nWindows, windowOffset, ...)
      standardGeneric('RectangularWindows'))

# This method returns a list of [nWindows], each of duration [windowDur],
# spaced evenly throughout the interval of the [waveform] between [from]
# and [to].
setMethod(
  f   = 'RectangularWindows',
  sig = c(waveform     = 'character',
          from         = 'numeric',
          to           = 'numeric',
          windowDur    = 'numeric',
          nWindows     = 'numeric',
          windowOffset = 'missing'),
  def = function(waveform, from, to, windowDur, nWindows, preserveTimes = TRUE) {
    xMins <- seq(from   = from,
                 to     = to - windowDur,
                 length = nWindows)
    xMaxs <- seq(from   = from + windowDur,
                 to     = to,
                 length = nWindows)
    Map(Waveform,
        waveform      = waveform,
        from          = xMins,
        to            = xMaxs,
        preserveTimes = preserveTimes)
  })


#############################################################################
#  RMS amplitude                                              RMS amplitude #
#############################################################################

# rms
if (! isGeneric('rms'))
  setGeneric(
    name = 'rms',
    def  = function(x) standardGeneric('rms'))

setMethod(
  f   = 'rms',
  sig = c(x = 'Waveform'),
  def = function(x) sqrt(mean(samples(x) ^ 2)))

setMethod(
  f   = 'rms',
  sig = c(x = 'list'),
  def = function(x) Reduce(c, Map(rms, x)))


#############################################################################
#  @samplePeriod                                              @samplePeriod #
#############################################################################

# samplePeriod
if (! isGeneric('samplePeriod'))
  setGeneric(
    name = 'samplePeriod',
    def  = function(x) standardGeneric('samplePeriod'))

setMethod(
  f   = 'samplePeriod',
  sig = c(x = 'Waveform'),
  def = function(x) 
    1 / sampleRate(x))


#############################################################################
#  @sampleRate get-method                                       @sampleRate #
#############################################################################

# sampleRate
if (! isGeneric('sampleRate'))
  setGeneric(
    name = 'sampleRate',
    def  = function(x) standardGeneric('sampleRate'))

setMethod(
  f   = 'sampleRate',
  sig = c(x = 'Waveform'),
  def = function(x) x@sampleRate)

#############################################################################
#  @samples get-method                                             @samples #
#############################################################################

# samples
if (! isGeneric('samples'))
  setGeneric(
    name = 'samples', 
    def  = function(x) standardGeneric('samples'))

setMethod(
  f   = 'samples', 
  sig = c(x = 'Waveform'), 
  def = function(x) x@samples)


#############################################################################
#  sampleTimes                                                  sampleTimes #
#############################################################################

# sampleTimes
if (! isGeneric('sampleTimes'))
  setGeneric(
    name = 'sampleTimes',
    def  = function(x) standardGeneric('sampleTimes'))

setMethod(
  f   = 'sampleTimes',
  sig = c(x = 'Waveform'),
  def = function(x) 
    seq(from = startTime(x), by = samplePeriod(x), length.out = N(x)))

# times
if (! isGeneric('times'))
  setGeneric(
    name = 'times',
    def  = function(x) standardGeneric('times'))

setMethod(
  f   = 'times',
  sig = c(x = 'Waveform'),
  def = function(x) sampleTimes(x))


#############################################################################
#  show                                                                show #
#############################################################################

setMethod(
  f   = 'show',
  sig = c(object = 'Waveform'),
  def = function(object) {
    message(sprintf('Number of samples: %d', N(object)))
    message(sprintf('Sample rate:       %d', sampleRate(object)))
    message(sprintf('Bit rate:          %d', bitRate(object)))
    wave <- data.frame(Amplitude = samples(object)[1:N(object)], 
                       Time = sampleTimes(object))
    xlimits <- c(wave$Time[1], wave$Time[nrow(wave)])
    xbreaks <- seq(from = xlimits[1], to = xlimits[2], length.out = 4)
    xlabels <- sprintf('%0.4f', xbreaks)
    print(
      ggplot(data = wave, aes(x = Time, y = Amplitude)) + 
        geom_path(colour = 'black') + theme_bw() + xlab('Time (s)') +
        scale_x_continuous(limits = xlimits, breaks = xbreaks, labels = xlabels)
    )
  })


#############################################################################
#  @startTime get-method                                         @startTime #
#############################################################################

# startTime
if (! isGeneric('startTime'))
  setGeneric(
    name = 'startTime',
    def  = function(x) standardGeneric('startTime'))

setMethod(
  f   = 'startTime',
  sig = c(x = 'Waveform'),
  def = function(x) x@startTime)


#############################################################################
#  Zero-padding a Waveform                                          ZeroPad #
#############################################################################

# ZeroPad
if (! isGeneric('ZeroPad'))
  setGeneric(
    name = 'ZeroPad',
    def  = function(x, ...) standardGeneric('ZeroPad'))

setMethod(
  f   = 'ZeroPad',
  sig = c(x = 'Waveform'),
  def = function(x, lengthOut, everyHz = NULL) {
    if (! is.null(everyHz)) {
      lengthOut <- sampleRate(x) / everyHz
      if (lengthOut %% 1 != 0) {
        lengthOut <- ceiling(lengthOut)
        message(sprintf('Padding to %d samples (i.e., every %f Hz)'),
                lengthOut, sampleRate(x)/lengthOut)
      }
    }
    lengthOut <- max(lengthOut, N(x))
    .pad      <- rep(0, lengthOut - N(x))
    x@samples <- c(x@samples[1:N(x)], .pad)
    return(x)
  })

# padded
if (! isGeneric('padded'))
  setGeneric(
    name = 'padded',
    def  = function(x) standardGeneric('padded'))

setMethod(
  f   = 'padded',
  sig = c(x = 'Waveform'),
  def = function(x) length(x) - N(x))


################################################################################
# Waveform arithmetic   ########################################################
################################################################################

# add 2 Waveform objects
setMethod(
  f   = '+',
  sig = c(e1 = 'Waveform', e2 = 'Waveform'),
  def = function(e1, e2) {
    .compute <- TRUE
    if (N(e1) != N(e2)) {
      message(sprintf('%s (N = %d) and %s (N = %d) must have the same number of samples to be added.',
                      deparse(substitute(e1)), N(e1),
                      deparse(substitute(e2)), N(e2)))
      .compute <- FALSE
    }
    if (length(e1) != length(e2)) {
      message(sprintf('%s (length = %d) and %s (length = %d) must have the same length to be added.',
                      deparse(substitute(e1)), length(e1),
                      deparse(substitute(e2)), length(e2)))
      .compute <- FALSE
    }
    if (sampleRate(e1) != sampleRate(e2)) {
      message(sprintf('%s (rate = %d) and %s (rate = %d) must have the same sample rate to be added.',
                      deparse(substitute(e1)), sampleRate(e1),
                      deparse(substitute(e2)), sampleRate(e2)))
      .compute <- FALSE
    }
    if (.compute) {
      .samples <- samples(e1) + samples(e2)
      new(Class      = 'Waveform',
          samples    = .samples,
          bitRate    = bitRate(e1),
          sampleRate = sampleRate(e1),
          startTime  = startTime(e1),
          N          = N(e1))
    } else {
      return(NULL)
    }
  }
)

# subtract 2 Waveform objects
setMethod(
  f   = '-',
  sig = c(e1 = 'Waveform', e2 = 'Waveform'),
  def = function(e1, e2) {
    .compute <- TRUE
    if (N(e1) != N(e2)) {
      message(sprintf('%s (N = %d) and %s (N = %d) must have the same number of samples to be subtracted.',
                      deparse(substitute(e1)), N(e1),
                      deparse(substitute(e2)), N(e2)))
      .compute <- FALSE
    }
    if (length(e1) != length(e2)) {
      message(sprintf('%s (length = %d) and %s (length = %d) must have the same length to be subtracted.',
                      deparse(substitute(e1)), length(e1),
                      deparse(substitute(e2)), length(e2)))
      .compute <- FALSE
    }
    if (sampleRate(e1) != sampleRate(e2)) {
      message(sprintf('%s (rate = %d) and %s (rate = %d) must have the same sample rate to be subtracted.',
                      deparse(substitute(e1)), sampleRate(e1),
                      deparse(substitute(e2)), sampleRate(e2)))
      .compute <- FALSE
    }
    if (.compute) {
      .samples <- samples(e1) - samples(e2)
      new(Class      = 'Waveform',
          samples    = .samples,
          bitRate    = bitRate(e1),
          sampleRate = sampleRate(e1),
          startTime  = startTime(e1),
          N          = N(e1))
    } else {
      return(NULL)
    }
  }
)


