 ## Yu and Davidson study of ants on tropical trees

n <- c(59, 7, 65, 24) # counts for tree species
o <- c(16, 5, 6, 12)  # observed occurrence of ants
p <- n / sum(n)       # expected proportion of ants by tree
p

?chisq.test
chisq.test(x=o, p=p)

