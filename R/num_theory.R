# R Package christian
#
# is.integer: Check for whole number
# is.prime: Check for prime number
# is.even: Check if number is even
# sieve: Sieve of Eratosthenes
# gcd: greatest common divisor
# lcm: least common multiple
# Pollard's rho algorithm
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
# http://librestats.com/2011/08/20/prime-testing-function-in-r/
is.prime <- function(n){ # n=Integer you want to know if is/not prime
  if ((n-floor(n)) > 0){
    stop("is.prime only accepts natural number inputs\n")
  } else if (n < 1){
    stop("is.prime only accepts natural number inputs\n")
  } else
    # Prime list exists
    if (try(is.vector(primes), silent=TRUE) == TRUE){
      # Prime list is already big enough
      if (n %in% primes){
        TRUE
      } else
        if (n < tail(primes,1)){
          FALSE
        } else
          if (n <= (tail(primes,1))^2){
            flag <- 0
            for (prime in primes){
              if (n%%prime == 0){
                flag <- 1
                break
              }
            }
            if (flag == 0){
              TRUE
            }
            else {
              FALSE
            }
          }
      # Prime list is too small; get more primes
      else {
        last.known <- tail(primes,1)
        while ((last.known)^2 < n){
          assign("primes", c(primes,next.prime(primes)), envir=.GlobalEnv)
          last.known <- tail(primes,1)
        }
        is.prime(n)
      }
    } else {
      # Prime list does not exist
      # *****************************************************************
      # Should this not rather use sieve instead of primes.below?
      # sieve is much much faster!
      # *****************************************************************
      assign("primes", primes.below(n,below.sqrt=TRUE), envir=.GlobalEnv)
      is.prime(n)
    }
}

next.prime <- function(primes){ # primes=Known prime list
  i <- tail(primes,1)
  while (TRUE){
    flag <- 0
    i <- i+2
    if (i%%6 == 3){
      flag <- 1
    }
    if (flag == 0){
      s <- sqrt(i)+1
      possible.primes <- primes[primes<s]
      for (prime in possible.primes){
        if ((i%%prime == 0)){
          flag <- 1
          break
        }
      }
      if (flag == 0){
        break
      }
    }
  }
  i
}

primes.below <- function(n, below.sqrt=FALSE){
  if (below.sqrt == TRUE){
    m <- ceiling(sqrt(n))
  } else {
    m <- n
  }

  primes <- c(2,3)
  i <- 3
  while (i < m-1){
    flag <- 0
    i <- i+2
    if (i%%6 == 3){
      flag <- 1
    }
    if (flag == 0){
      s <- sqrt(i)+1
      possible.primes <- primes[primes<s]
      for (prime in possible.primes){
        if ((i%%prime == 0)){
          flag <- 1
          break
        }
      }
      if (flag == 0){
        primes <- c(primes, i)
      }
    }
  }
  primes
}

# Sieve of Eratosthenes
# http://stackoverflow.com/questions/3789968/generate-a-list-of-primes-in-r-up-to-a-certain-number
sieve<- function(n)
{
  n <- as.integer(n)
  if(n > 1e8) stop("n too large")
  primes <- rep(TRUE, n)
  primes[1] <- FALSE
  last.prime <- 2L
  fsqr <- floor(sqrt(n))
  while (last.prime <= fsqr)
  {
    primes[seq.int(2L*last.prime, n, last.prime)] <- FALSE
    sel <- which(primes[(last.prime+1):(fsqr+1)])
    if(any(sel)){
      last.prime <- last.prime + min(sel)
    }else last.prime <- fsqr+1
  }
  which(primes)
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

# Pollard's rho algorithm
# see https://en.wikipedia.org/wiki/Pollard's_rho_algorithm
pollard_rho <- function(n){
  if (is.posint(n)){
    if (n==1){
      return(1)
    }
    x_fixed<-2
    cycle_size<-2
    x<-2
    factor<-1
    while (factor==1){
      i<-1
      while(i<=cycle_size && factor<=1){
        x<-(x*x+1)%%n
        factor<-gcd(x-x_fixed,n)
        i<-i+1
      }
      cycle_size<-cycle_size*2
      x_fixed<-x
    }
    return(factor)
  }
  else stop ("pollard_rho requires a positive integer")
}

# Factorisation
# http://rosettacode.org/wiki/Prime_decomposition#R
factorize <- function(n) {
  d <- c()
  div <- 2; nxt <- 3; rest <- n
  while( rest != 1 ) {
    while( rest%%div == 0 ) {
      d <- c(d, div)
      rest <- floor(rest / div)
    }
    div <- nxt
    nxt <- nxt + 2
  }
  d
}

# simple divisors function
# to be deleted
simple_divisors <- function(n) {
  n <- as.integer(n)
  div <- seq_len(abs(n))
  factors <- div[n %% div == 0L]
  return(factors)
}

