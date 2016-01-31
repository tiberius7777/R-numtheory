\name{is.prime}
\alias{is.prime}
\title{Check for Prime Number}
\description{
Checks whether n is a prime number.
}
\usage{
is.prime(n)
}
\arguments{
  \item{n}{A non-negative integer}
}
\details{
  \enumerate{is.prime returns false for n=0.}
  \enumerate{is.prime returns an error for n<0.}
}
\examples{
is.prime(3) # TRUE
is.prime(2) # FALSE
}
