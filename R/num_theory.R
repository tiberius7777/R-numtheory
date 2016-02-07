# is.prime: Check for prime number
# is.even: Check if number is even
# sieve: Sieve of Eratosthenes
# gcd: greatest common divisor
# lcm: least common multiple
# pollard_rho: Pollard's rho algorithm
# factorize: Factorisation of an integer
# divisors: List of all integer divisors of an integer number
# tau: number of all divisors of an integer n
# sigma: sum of all divisors of an integer n
# primorial: factorial of all prime numbers smaller than n
# Euler's totient function phi
# legendre: Legenre Symbol
# Euclid: Extended Euclidean Algorithm to solve a system of linear congruences

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# check if n is even
is.even <- function(n) {
  return(ifelse((n %% 2) == 0, TRUE,FALSE))
}

# check if number is a whole number
# n=0 returns false
is.posint <- function(n){
#  if (n==0) {
#    FALSE
#  }
#  else {
#    !grepl("[^[:digit:]]", format(n,  digits = 20, scientific = FALSE))
#  }
  (is.int(n) && (n>0))
}

# check if number is a whole number
# workaround because base::is.integer(n) does not check if n is stored as an integer
is.int <- function(n){
  n == round(n)
}

# check if number is a prime number
# simple algorithm, see http://librestats.com/2011/08/20/prime-testing-function-in-r/
# AKS algorithm, see http://math.stackexchange.com/questions/204560/implementing-aks-primality-prover
# accuracy parameter needed for Miller-Rabin test
# Miller-Rabin test not yet functional
is.prime <- function(n, algorithm = c("Simple", "Miller-Rabin"), accuracy=7){ # n=Integer you want to know if is/not prime
  # evaluate parameters
  algorithm <- match.arg(algorithm)
  if (algorithm == "Simple"){
    if ((n-floor(n)) > 0){
      stop("is.prime only accepts natural number inputs\n")
      }
      else if (n < 1){
        stop("is.prime only accepts natural number inputs\n")
      }
      else
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
    else if (algorithm=="Miller-Rabin"){
      s<-0
      d<-n-1
      while(d%%2 == 0){
        d<-d%/%2
        s<-s+1
      }
      for (i in 0:accuracy-1){
        a<-as.integer(runif(1,2,n-2))
        x<-a^d %% n
        if(!((x==1)||(x==(n-1)))){
          r<-1
          for(r in 1:(s-1)){
            x<-x^2%%n
            if(x==1)
              return(FALSE)
            if(x==(n-1)){
              a<-0
              break
            }
          }
          if (r==s)
            return(FALSE)
        }
        return(TRUE)
      }
    }
    else stop("No primality algorithm defined")
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
  else stop ("requires a positive integer")
}

# Factorisation
# http://rosettacode.org/wiki/Prime_decomposition#R
factorize <- function(n) {
  d <- c()
  div <- 2; nxt <- 3; rest <- n
  while( rest != 1 ) {
    while( rest%%div == 0 ) {
        d<-c(d, div)
      rest <- floor(rest / div)
    }
    div <- nxt
    nxt <- nxt + 2
  }
  d
}

divisors <-function(n){
  if (is.posint(n)){
    simple_divisors(n) #alt
  }
  else stop ("requires a positive integer")
}

# simple divisors function, works until 10⁹
simple_divisors <- function(n) {
  if (is.posint(n)){
    n <- as.integer(n)
    div <- seq_len(abs(n))
    factors <- div[n %% div == 0L]
    return(factors)
  }
  else stop ("requires a positive integer")
}

# tau: number of all divisors of an integer n
tau<-function(n){
  if (is.posint(n)){
    return(length(divisors(n)))
  }
  else stop ("requires a positive integer")
}

# sigma: sum of all divisors of an integer n
sigma<-function(n){
  if (is.posint(n)){
    sum(divisors(n))
  }
else stop ("requires a positive integer")
}

#primorial:
primorial <- function(n){
  if (is.posint(n)){
    prod(sieve(n))
  }
  else stop ("requires a positive integer")
}

# Euler's totient function phi
phi <- function(n){
  if (is.posint(n)){
    result <- n
    p <- unique(factorize (n))
    for (i in p){
     #if (i>1){
      while(n%%i==0){
        n <- n%/%i
      }
      result <- (result - result %/% i)
    #}
    }
    if (n>1)
      result = (result - result %/% n)
    return(result)
  }
  else stop ("requires a positive integer")
}


# Legendre function, a multiplicative function with values 1, -1, 0 that is a
# quadratic character modulo a prime number p.
# Its value on a nonzero quadratic residue mod p is 1 and on a non-quadratic
# residue is -1. Its value on zero is 0
# See also https://martin-thoma.com/calculate-legendre-symbol/

legendre <- function(a, p){
  if (is.int(a) && (p>=3)){
    if ((a>=p) || (a<0)){
      #cat("a>=p, calling legendre(a=", a%%p, ",p=",p, "\n")
      legendre(a%%p,p)
    }
    else if ((a==0) || (a==1)){
      return(a)
    }
    else if (a==2){
      if ((p%%8==1) || (p%%8 ==7)){
        return(1)
      }
      else return(-1)
    }
    else if (a==p-1){
      if (p%%4==1){
        #cat("a==p-1 --> 1\n")
        return(1)
      }
      else{
        #cat("a==p-1 --> -1\n")
        return(-1)
      }
    }
    else if (!is.prime(a)){
      #cat("a not prime\n")
      factors<-factorize(a)
      product <- 1
      for (pi in factors){
        product<-product*legendre(pi,p)
      }
      return(product)
    }
    else{
      if ( (((p-1)%/%2)%%2==0) || (((a-1)%/%2)%%2==0)){
        #cat("Calling legendre(a=", p, ",p=",a, "\n")
        legendre(p,a)
      }
      else{
        #cat("Calling (-1)*legendre(a=", p, ",p=",a, "\n")
        (-1)*legendre(p,a)
      }
    }
  }
  else{
    stop("requires one integer and one prime number >=3 as input")
  }
}

# Modular square root
# y²=x mod n
msqrt <- function (x,n){

}

# Extended Euclidean Algorithm
# to solve a system of linear congruences
# see https://martin-thoma.com/solve-linear-congruence-equations/
ext_euclid <- function(a,b){
  aO <- a
  bO <- b

}

