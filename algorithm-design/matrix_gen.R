permutation_matrix <-
function(nrow = 1, ncol = 1) {
    data <- permutations(nrow, ncol)
    mat <- matrix(data, nrow, ncol, byrow=TRUE)
    return(mat)
}

permutations <-
function(n, size) {
    prefs <- NULL
    for (x in 1:n) {
        prefs <- c(prefs, sample(1:size))
    }
    return(prefs)
}
