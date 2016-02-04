\name{Euler phi function}
\alias{Euler phi function}
\title{Euler phi function}
\description{
Euler’s Phi function φ(n), also known as Euler’s ‘totient’ function.
}
\usage{
phi(n)
}
\arguments{
  \item{n}{A non-negative integer}
}
\details{
  The phi function φ(n) is defined as the number of integers k with 1 ≤ k ≤ n and coprime to n.

  Leonhard Euler introduced the function in 1763. He did not at that time choose any specific symbol to denote it. In a 1784 publication, Euler studied the function further, choosing the Greek letter π to denote it: he wrote πD for "the multitude of numbers less than D, and which have no common divisor with it".

  The standard notation φ(A) comes from Gauss's 1801 treatise Disquisitiones Arithmeticae. However, Gauss didn't use parentheses around the argument and wrote φA. Thus, it is often called Euler's phi function or simply the phi function.

In 1879, J. J. Sylvester coined the term totient for this function, so it is also referred to as Euler's totient function or the Euler totient.
}
\examples{
  phi(7) = 6        # for prime numbers, phi(p)=p-1
  phi(8) = 4        # the numbers relatively prime to 8 are 1, 3, 5, 7
}
