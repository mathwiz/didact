?t.test

## One sample compared to mu=0
dat1 <- rnorm(20, 1, 2)
t.test(x=dat1, mu=0, conf.level=0.95)

## Another single sample
dat2 <- rnorm(20, 1.3, 2)
t.test(x=dat2, mu=0, conf.level=0.95)

## Comparing two samples for different means
t.test(dat1, dat2, mu=0, paired=F, var.equal=T, conf.level=0.95)

## Try another sample to compare
dat3 <- rnorm(30, 3, 3)
t.test(dat1, dat3, mu=0, paired=F, var.equal=F, conf.level=0.95)

## Try a non-parametric test
wilcox.test(dat1, dat3)

