\name{legendre}
\alias{legendre}
\title{Legendre Symbol for quadratic residues}
\description{
The Legendre Symbol (a/p), where p must be a prime number, denotes whether a is a quadratic residue modulo p or not.
The function returns 1 if a is a quadratic residue mod p, -1 if a is a quadratic non-residue mod p, and 0 is a is congruent to 0 mod p.
A is a quadratic non-residue mod p if it is not a multiple of p and does not have a square root mod p.
}
\usage{
legendre(a,p)
}
\arguments{
  \item{a}{ integer}
  \item{p}{ prime number}
}
\details{
  Functions throws an error if a is not an integer and p is not a prime number.

  The Legendre symbol was introduced by Adrien-Marie Legendre in 1798 in the course of his attempts at proving the law of quadratic reciprocity. Generalizations of the symbol include the Jacobi symbol and Dirichlet characters of higher order.

  See also \url{https://en.wikipedia.org/wiki/Legendre_symbol}
}
\value{
  Returns 0, 1, or -1 if p divides a, a is a quadratic residue, or if not.
}
\examples{
legendre(74,101)     = -1
legendre(3,73)       = 1
legendre(22,11)      = 0
legendre(5,2)        = -1
legendre(-2342,1901) = 1
}
