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
  is.prime expects a positive integer and returns an error for \eqn{n \le 0}. Works well until 10⁶. Algorithm is based on \url{http://librestats.com/2011/08/20/prime-testing-function-in-r/}
}
\examples{
is.prime(3) # TRUE
is.prime(2) # FALSE
}
