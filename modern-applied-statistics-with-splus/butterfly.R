butterfly <- function () {
    theta <- seq(0, 24*pi, len=2000)
    radius <- exp(cos(theta)) - 2*cos(4*theta) + sin(theta/12)^5
    plot(radius*sin(theta), -radius*cos(theta), type="l", axes=F, xlab="", ylab="")
}
