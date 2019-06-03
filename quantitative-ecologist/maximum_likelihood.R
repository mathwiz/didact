## Wildebeest sex ratio example (a binomial process, i.e. birth of a male)

N <- 10 # number of trials
n <- 4 # number of male calves

## Theoretical maximum likelihood of a binomial process with prob=p is n/N

## Function to calculate maximum likelihood
f <- function(N, n, p) {
    log(dbinom(n,N,p))
}

## Plot peak of likelihood
ps <- seq(0,1,0.01)
plot(ps, f(N,n,ps), type="l", ylab="log-likelihood", xlab="p")
abline(v=0.4, lty="dashed") # theoretical maximum


## Estimate maximum likelihood
g <- optimize(f, lower=0, upper=1, N=N, n=n, maximum=T)
g


## Vole weight example

## Estimate multiple parameters via maximum likelihood
w <- c(32.5, 38.3, 33.7, 32.3, 31.4, 24.8, 26.2, 26.2, 27.8, 26.0)
f <- function(p, w) {
    mu <- p[1]
    si <- p[2]
    -sum(log(dnorm(w, mu, si)))
}

g <- optim(fn=f, par=c(mu=30, si=4), w=w)
g

