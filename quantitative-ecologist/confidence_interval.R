normInf <- function(dat, clevel) {
    n <- length(dat)
    av <- mean(dat)
    s <- sd(dat)
    se <- s/sqrt(n)
    p <- (1 - clevel) / 2 # lower and upper tail prob
    t1 <- qt(p, n-1) # lower t value
    t2 <- qt(p, n-1, lower.tail=F) # upper t value
    chi1 <- qchisq(p, n-1) # lower chisq
    chi2 <- qchisq(p, n-1, lower.tail=F) # upper chisq

    muCI <- round(c(av - t2*se, av - t1*se), 2)
    varCI <- round(c((n - 1)*s^2 / chi2, (n - 1)*s^2 / chi1), 2)

    print(paste("Mean is estimated as", round(av, 2), "with ", clevel*100, "%CI (", muCI[1], ", ", muCI[2], ")"))
    print(paste("Variance is estimated as", round(s^2, 2), "with ", clevel*100, "%CI (", varCI[1], ", ", varCI[2], ")"))}

diceRolls <- function(n) {
    sample(1:6, n, replace=T) + sample(1:6, n, replace=T)
}

dat <- diceRolls(30)
dat
normInf(dat, 0.95)

