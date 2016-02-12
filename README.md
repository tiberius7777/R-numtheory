# R-numtheory
R package that provides number theory functions, optimized for performance on large numbers.
## Current status
At the moment this is still in experimental stage. I am using this as a replacement for Maple which I find difficult to use, therefore the plan is to re-engineer standard Maple functions and more exotic ones in standard R syntax, without linking any further code. Focus will be on factorization of large numbers and integer divisors.
Please use this package at your own risk. There's a reason why this has not been uploaded to CRAN yet.

Please feel free to contribute, though.

##Functions implemented so far:
- is.int(n): Check if numer is a whole number, as a workaround given that base::is.integer doees not do this
- is.posint(n): Check for positive whole number
- is.prime(n): Check for prime number, working well for numbers up to 10¹¹.
- is.even(n): Check if number is even
- sieve(n): Sieve of Eratosthenes, optimized for large numbers up to 10⁸
- gcd(a,b): Greatest common divisor
- lcm(a,b): Least common multiple
- pollard_rho(n): Pollard's rho algorithm for prime factorization of large numbers
- divisors(n): List of all integer divisors of integer n. Works well up to around 10⁹.
- tau(n): number of divisors of n. Works well up to around 10⁹.
- sigma(n): sum of all divisors of n. Works well up to around 10⁹.
- primorial(n): factorial of all prime numbers < n. Works well up to n=700.
- phi(n): Euler's Phi function (also known as Euler's "totient"" function). Works well up to 10¹¹
- legendre(a,p): Legendre symbol for quadratic residues
- igcdex(a,b): Extended Euclidean Algorithm to find s and t such that gcd(a,b) = sa + tb

##To do:
- Within the is.prime function, should 'sieve(n)' be used rather than 'primes.below'? Should improve performance
- Check for prime numbers > 10¹¹
- AKS Primality Test
- Rabin-Miller Primality Test
- Modular Square Root
- Jacobi symbol

