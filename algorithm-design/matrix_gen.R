permutation_matrix <-
function(nrow = 1, ncol = 1) {
    return (matrix(permutations(nrow, ncol), nrow, ncol, byrow=TRUE))
}

permutations <-
function(n, size) {
    prefs <- NULL
    for (x in 1:n) {
        prefs <- c(prefs, sample(1:size))
    }
    return(prefs)
}
