\name{igcdex}
\alias{igcdex}
\title{igcdex - extended Euclidean algorithm for integers}
\description{
Extended Euclidean algorithm for integers
}
\usage{
igcdex(a,b)
}
\arguments{
  \item{a}{integer}
  \item{b}{integer}
}

\details{
   igcdex returns g = igcd(a, b) such that g=sa+tb.
}
\examples{
igcdex(35,50) = 3 -2
igcdex(1,2)   = 1  0
}
