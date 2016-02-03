# R-numtheory
R Package that provides number theory functions, optimized for performance on large numbers.
## Current status
At the moment this is still in experimental stage. I am using this as a replacement for Maple which I find difficult to use, therefore the plan is to re-engineer standard Maple functions and more exotice ones in standard R syntax, without linking any further code. Focus will be on factorization of large numbers and integer divisors.
Please use this package at your own risk. There's a reason why this has not been uploaded to CRAN yet.

Please feel free to contribute, though.

##Functions implemented so far:
- is.integer(n): Check for whole number
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

##To do:
- Within the is.prime function, should 'sieve(n)' be used rather than 'primes.below'? Should improve performance
- Check for prime numbers > 10¹¹
- phi(n): Euler's Phi function (aka Euler's "totient"" function)
- AKS Primality Test
- Rabin-Miller Primality Test

