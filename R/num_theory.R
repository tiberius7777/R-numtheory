# R Package christian
#
# is.integer: Check for whole number
# is.prime: Check for prime number
# is.even: Check if number is even
# primest: Sieve of Eratosthenes
# gcd: greatest common divisor
# lcm: least common multiple
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

# prüfen, ob Zahl n gerade ist
is.even <- function(n) {
  return(ifelse((n %% 2) == 0, TRUE,FALSE))
}

# check if number is a whole number
# n=0 returns false
is.posint <- function(n){
  if (n==0) {
    FALSE
  }
  else {

    !grepl("[^[:digit:]]", format(n,  digits = 20, scientific = FALSE))
  }
}

# check if number is a prime number
is.prime <- function(n) {
  if (is.posint(n)) {
    if (n == 2) {
      TRUE
    }
    else if (any(n %% 2:(n-1) == 0)) {
      FALSE
    }
    else{
      TRUE
    }
  }
  else{
    stop('The argument "n" must be a positive integer')
  }
}

# Sieve of Eratosthenes
primest <- function(n){
  p <- 2:n
  i <- 1
  while (p[i] <= sqrt(n)) {
    p <-  p[p %% p[i] != 0 | p==p[i]]
    i <- i+1
  }
  p
}

# Greatest common divisor
gcd <- function(a,b) {
  if (!(a==0 && b==0)){
    if ( a == 0 )
      return(b)
    if ( b == 0 )
      return(a)
    r <- a%%b;
    return(ifelse(r, gcd(b, r), b))
  }
  else stop('greatest common divisor not defined for (a,b)=(0,0)!')
}

# least common multiple
lcm <- function(a, b)
{
  if (!(a==0 && b==0)){
  return(abs(a*b)/gcd(a,b))
  }
  else stop('least common multiple not defined for (a,b)=(0,0)!')
}
