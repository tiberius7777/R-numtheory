\name{pollard_rho}
\alias{pollard_rho}
\title{Pollard' Rho algorithm for prime factorization}
\description{
Pollard's rho algorithm is a special-purpose integer factorization algorithm, invented by John Pollard in 1975. It is particularly effective for a composite number having a small prime factor.

The algorithm is very fast for numbers with small factors, but slower in cases where all factors are large. The algorithm's most remarkable success was the factorization of the eighth Fermat number, F_8 = 1238926361552897 * 93461639715357977769163558199606896584051237541638188580280321. The algorithm was a good choice for F8 because the prime factor p = 12389263661552897 is much smaller than the other factor. The factorization took 2 hours on a UNIVAC 1100/42.
}
\usage{
pollard_rho(n)
}
\arguments{
  \item{n}{A non-negative integer > 1}
}
\details{
  See \url{https://en.wikipedia.org/wiki/Pollard's_rho_algorithm} for more details.
}
\examples{
pollard_rho(8051)     = 97
pollard_rho(10403)    = 101
pollard_rho(62615533) = 7907
}
