% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/decay_exponential.R
\name{decay_exponential}
\alias{decay_exponential}
\title{Negative exponential decay function}
\usage{
decay_exponential(decay_value)
}
\arguments{
\item{decay_value}{A \code{numeric}. The calibration parameter that, when
multiplied by the travel cost, is used as the exponent of \code{e} in the
negative exponential function.}
}
\value{
A \code{function} that takes a generic travel cost vector (\code{numeric}) as
an input and returns a vector of weights (\code{numeric}).
}
\description{
Returns a negative exponential weighting function to be used inside
accessibility calculating functions.

This function is generic over any kind of numeric travel cost,
such as distance, time and money.
}
\examples{
weighting_function <- decay_exponential(decay_value = 0.1)

weighting_function(20)

weighting_function(35)

}
\seealso{
Other decay functions: 
\code{\link{decay_binary}()},
\code{\link{decay_linear}()},
\code{\link{decay_power}()},
\code{\link{decay_stepped}()}
}
\concept{decay functions}
