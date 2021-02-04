# Author: Patrick Reidy
# THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
# INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
# PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
# HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF
# CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE
# OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.





################################################################################
# Class definition                                           Periodogram class #
################################################################################

setClass(
  Class = 'Periodogram',
  contains = c('Spectrum'))



################################################################################
# Object construction                                      Periodogram objects #
################################################################################

setGeneric(
  name = 'Periodogram',
  def  = function(x, ...) standardGeneric('Periodogram'))

setMethod(
  f   = 'Periodogram',
  sig = c(x = 'Waveform'),
  def = function(x) {
    # Use attributes of the Waveform x to determine Spectrum attributes.
    .nyquist     <- sampleRate(x) / 2
    .bin.width   <- sampleRate(x) / length(samples(x))
    .frequencies <- seq(from = 0, to = .nyquist, by = .bin.width)
    # Compute the periodogram ordinates.
    .ordinates   <- (1 / (N(x)*samplePeriod(x))) * abs(fft(samples(x)))^2
    .ordinates   <- .ordinates[1:length(.frequencies)]
    # Construct a new Periodogram object.
    new(Class = 'Periodogram',
        values   = .ordinates,
        binWidth = .bin.width,
        nyquist  = .nyquist)
  })

