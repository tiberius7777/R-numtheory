# R-numtheory
R Package that provides number theory functions, optimized for large numbers
## Current status
At the moment this is still in experimental stage. I am using this as I'm going through uni at the moment. Uni recommends Maple which I find difficult to use, therefore the plan is to re-engineer standard Maple functions and more exotice ones for R. Focus will be on factorization of large numbers and integer divisors.
Please use this package at your own risk. There's a reason why this has not been uploaded to CRAN yet.
Please feel free to contribute, though.
##Functions implemented so far:
- is.integer(n): Check for whole number
- is.prime(n): Check for prime number, optimized for numbers up to 10⁶.
- is.even(n): Check if number is even
- sieve(n): Sieve of Eratosthenes, optimized for large numbers
- gcd(a,b): Greatest common divisor
- lcm(a,b): Least common multiple
- pollard_rho(n): Pollard's rho algorithm for prime factorization of large numbers

##To do:
- Within the is.prime function, can sieve(n) be used rather than primes.below. Should improve performance
- Check for prime numbers > 10⁶
- divisors: list of divisors of an integer
- tau(n): number of divisors of n
- sigma(n): sum of all divisors of n
- phi(n): Euler's Phi function (aka Euler's "totient"" function)

##Contact Details
christian@bessenroth.de
