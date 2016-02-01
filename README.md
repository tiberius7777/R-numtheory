# R-numtheory
R Package that provides number theory functions
## Current status
At the moment this is still in experimental stage. I am using this as I'm going through uni at the moment. Uni uses Maple which I find difficult to use, therefore the plan is to re-engineer standard Maple functions for R.
Please use this package at your own risk. There's a reason why this has not been uploaded to CRAN (yet).
Please feel free to contribute, though.
##Functions implemented so far:
- is.integer(n): Check for whole number
- is.prime(n): Check for prime number, optimized for large numbers
- is.even(n): Check if number is even
- sieve(n): Sieve of Eratosthenes
- gcd(a,b): Greatest common divisor
- lcm(a,b): Least common multiple
- pollard_rho(n): Pollard's rho algorithm for prime factorization of large numbers
