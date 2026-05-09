# base functions
rest <- function(x) { x[-1] }
first <- function(x) { x[1] }

is.empty <- function(x) { length(x) == 0 }

sum.int <-
function(begin, end) {
    if (begin > end)
        return (0)
    else
        return (begin + sum.int(begin + 1, end))
}



