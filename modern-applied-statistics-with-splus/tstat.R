t.stat <- function(x, mu=0) {
    n <- length(x)
    t <- sqrt(n) * (mean(x) - mu) / sd(x)
    list(t = t, p = 2 * (1 - pt(abs(t), n-1)))
}
