# Author: Patrick Reidy
# THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
# INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
# PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
# HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF
# CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE
# OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.





################################################################################
# Class definition                                              Spectrum class #
################################################################################

setClass(
  Class = 'Spectrum',
  contains = c(),
  slots    = c(values   = 'numeric',
               binWidth = 'numeric',
               nyquist  = 'numeric')
  )




################################################################################
# Methods                                                     Spectrum methods #
################################################################################

#############################################################################
# @binWidth get-method                                             binWidth #
#############################################################################

# binWidth
if (! isGeneric('binWidth'))
  setGeneric(
    name = 'binWidth',
    def  = function(x) standardGeneric('binWidth'))

setMethod(
  f   = 'binWidth',
  sig = c(x = 'Spectrum'),
  def = function(x) x@binWidth)


#############################################################################
# Centroid frequency of a spectrum                                 centroid #
#############################################################################

# centroid
if (! isGeneric('centroid'))
  setGeneric(
    name = 'centroid',
    def  = function(x, ...) standardGeneric('centroid'))

setMethod(
  f   = 'centroid',
  sig = c(x = 'Spectrum'),
  def = function(x, minHz = 0, maxHz = Inf, scale = 'linear') {
    .freqs <- frequencies(x)[which(minHz <= freq(x) & freq(x) <= maxHz)]
    .vals  <- values(x)[which(minHz <= freq(x) & freq(x) <= maxHz)]
    if (scale == 'dB' | scale == 'decibel')
      .vals <- 10*log10(.vals/min(.vals))
    sum(.freqs * (.vals/sum(.vals)))
  })


#############################################################################
# Variance (second moment) of a spectrum                           variance #
#############################################################################

# variance
if (! isGeneric('variance'))
  setGeneric(
    name = 'variance',
    def  = function(x, ...) standardGeneric('variance')
  )
setMethod(
  f   = 'variance',
  sig = c(x = 'Spectrum'),
  def = function(x, minHz = 0, maxHz = Inf, scale = 'linear') {
    .inds  <- which(minHz <= frequencies(x) & frequencies(x) <= maxHz)
    .freqs <- frequencies(x)[.inds]
    .vals  <- values(x)[.inds]
    if (tolower(scale) %in% c('db', 'decibel'))
      .vals <- 10 * log10(.vals / min(.vals))
    sum((.vals / sum(.vals)) * ((.freqs - centroid(x, minHz, maxHz, scale))^2))
  }
)


#############################################################################
# Skewness (third moment) of a spectrum                            skewness #
#############################################################################

# skewness
if (! isGeneric('skewness'))
  setGeneric(
    name = 'skewness',
    def  = function(x, ...) standardGeneric('skewness')
  )
setMethod(
  f   = 'skewness',
  sig = c(x = 'Spectrum'),
  def = function(x, minHz = 0, maxHz = Inf, scale = 'linear') {
    .inds  <- which(minHz <= frequencies(x) & frequencies(x) <= maxHz)
    .freqs <- frequencies(x)[.inds]
    .vals  <- values(x)[.inds]
    if (tolower(scale) %in% c('db', 'decibel'))
      .vals <- 10 * log10(.vals / min(.vals))
    .top <- sum((.vals / sum(.vals)) * ((.freqs - centroid(x, minHz, maxHz, scale))^3))
    .bottom <- variance(x, minHz, maxHz, scale)^(3/2)
    .top / .bottom
  }
)


#############################################################################
# Kurtosis (fourth moment) of a spectrum                           kurtosis #
#############################################################################

# kurtosis
if (! isGeneric('kurtosis'))
  setGeneric(
    name = 'kurtosis',
    def  = function(x, ...) standardGeneric('kurtosis')
  )
setMethod(
  f   = 'kurtosis',
  sig = c(x = 'Spectrum'),
  def = function(x, minHz = 0, maxHz = Inf, scale = 'linear', excess = FALSE) {
    .inds  <- which(minHz <= frequencies(x) & frequencies(x) <= maxHz)
    .freqs <- frequencies(x)[.inds]
    .vals  <- values(x)[.inds]
    if (tolower(scale) %in% c('db', 'decibel'))
      .vals <- 10 * log10(.vals / min(.vals))
    .top <- sum((.vals / sum(.vals)) * ((.freqs - centroid(x, minHz, maxHz, scale))^4))
    .bottom <- variance(x, minHz, maxHz, scale)^2
    (.top / .bottom) - ifelse(excess, yes = 3, no = 0)
  }
)

#############################################################################
# Fourier frequencies of a spectrum                            fourierFreqs #
#############################################################################

# frequencies
if (! isGeneric('frequencies'))
  setGeneric(
    name = 'frequencies',
    def  = function(x) standardGeneric('frequencies'))

setMethod(
  f   = 'frequencies',
  sig = c(x = 'Spectrum'),
  def = function(x) 
    seq(from = 0, by = binWidth(x), length.out = length(values(x))))

# freq
if (! isGeneric('freq'))
  setGeneric(
    name = 'freq',
    def  = function(x) standardGeneric('freq'))

setMethod(
  f   = 'freq',
  sig = c(x = 'Spectrum'),
  def = function(x) frequencies(x))


#############################################################################
# Maximum amplitude                                           max amplitude #
#############################################################################

# maxAmp
if (! isGeneric('maxAmp'))
  setGeneric(
    name = 'maxAmp',
    def  = function(x, ...) standardGeneric('maxAmp'))

setMethod(
  f   = 'maxAmp',
  sig = c(x = 'Spectrum'),
  def = function(x, minHz = min(freq(x)), maxHz = max(freq(x)), scale = 'dB', reference = NULL) {
    .vals <- values(x)
    if (scale == 'dB') {
      if (is.null(reference))
        .vals <- 10*log10(.vals/max(.vals))
      else if (reference == 'max')
        .vals <- 10*log10(.vals/max(.vals))
      else if (reference == 'min')
        .vals <- 10*log10(.vals/min(.vals))
      else if (is.numeric(reference))
        .vals <- 10*log10(.vals/reference)
      else
        message('Reference power must be argument when using dB scale.')
    }
    .vals <- .vals[which(minHz <= frequencies(x) & frequencies(x) <= maxHz)]
    return(max(.vals))
  })


#############################################################################
# Minimum amplitude                                           min amplitude #
#############################################################################

# minAmp
if (! isGeneric('minAmp'))
  setGeneric(
    name = 'minAmp',
    def  = function(x, ...) standardGeneric('minAmp'))

setMethod(
  f   = 'minAmp',
  sig = c(x = 'Spectrum'),
  def = function(x, minHz = min(freq(x)), maxHz = max(freq(x)), scale = 'dB', reference = NULL) {
    .vals <- values(x)
    if (scale == 'dB') {
      if (is.null(reference))
        .vals <- 10*log10(.vals/max(.vals))
      else if (reference == 'max')
        .vals <- 10*log10(.vals/max(.vals))
      else if (reference == 'min')
        .vals <- 10*log10(.vals/min(.vals))
      else if (is.numeric(reference))
        .vals <- 10*log10(.vals/reference)
      else
        message('Reference power must be argument when using dB scale.')
    }
    .vals <- .vals[which(minHz <= frequencies(x) & frequencies(x) <= maxHz)]
    return(min(.vals))
  })


#############################################################################
# @nyquist get-method                                               nyquist #
#############################################################################

# nyquist
if (! isGeneric('nyquist'))
  setGeneric(
    name = 'nyquist',
    def  = function(x) standardGeneric('nyquist'))

setMethod(
  f   = 'nyquist',
  sig = c(x = 'Spectrum'),
  def = function(x) x@nyquist)


#############################################################################
# Peak frequency of a spectrum                                       peakHz #
#############################################################################

# peakHz
if (! isGeneric('peakHz'))
  setGeneric(
    name = 'peakHz',
    def  = function(x, ...) standardGeneric('peakHz'))

setMethod(
  f   = 'peakHz',
  sig = c(x = 'Spectrum'),
  def = function(x, minHz = 0, maxHz = Inf) {
    .inds <- which(minHz <= frequencies(x) & frequencies(x) <= maxHz)
    .freq <- frequencies(x)[.inds]
    .vals <- values(x)[.inds]
    .freq[which.max(.vals)]
  })


#############################################################################
#  show                                                                show #
#############################################################################

setMethod(
  f   = 'show',
  sig = c(object = 'Spectrum'),
  def = function(object) {
    .spec = data.frame(Frequency = frequencies(object),
                       Amplitude = 10*log10(values(object)/max(values(object))))
    print(
      ggplot(data = .spec, aes(x = Frequency, y = Amplitude)) +
        geom_path(colour = 'black') + theme_bw() +
        xlab('Frequency (Hz)') + ylab('Amplitude (dB)')
    )
  })


#############################################################################
# @values get-method                                                 values #
#############################################################################

# values
if (! isGeneric('values'))
  setGeneric(
    name = 'values',
    def  = function(x) standardGeneric('values'))

setMethod(
  f   = 'values',
  sig = c(x = 'Spectrum'),
  def = function(x) x@values)

# ordinates
if (! isGeneric('ordinates'))
  setGeneric(
    name = 'ordinates',
    def  = function(x) standardGeneric('ordinates'))

setMethod(
  f   = 'ordinates',
  sig = c(x = 'Spectrum'),
  def = function(x) values(x))
