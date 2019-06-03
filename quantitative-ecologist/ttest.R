?t.test

dat <- rnorm(20, 1, 2)
t.test(x=dat, mu=0, conf.level=0.95)

dat <- rnorm(20, 1.3, 2)
t.test(x=dat, mu=0, conf.level=0.95)

