rm(list=ls())

# Directional Selection using an Individual Locus model

# General assumptions
# 1. The trait under selection has two phenotypic expressions but is determined by an underlying normally distributed liability.
# 2. Only one morph is allowed to contribute to the next generation.

# Two morphs are coded 0 (not selected), 1 (selected)

selection = function(morph, genotype) {
    genotype[morph==1,]
}

mutation = function(xs, n.mutations, n.loci, n.ind) {
    row <- ceiling(runif(n.mutations, min=0, max=n.loci))
    temp <- matrix(xs)
    temp[row] <- abs(temp[row] - 1)
    return(matrix(temp, n.ind, n.loci))
}

gamete = function(xs, g.loci, n.loci) {
    sample(x=xs, size=g.loci, replace=F)
}    

#### MAIN PROGRAM ####
set.seed(100)

P = 10^-4               # mutation rate
N = 1000                # popluation size at each generation
G.Loci = 100            # loci per gamete
N.Loci = G.Loci * 2     # loci per individual
Tot.Loci = N * N.Loci
H2 = 0.5                # heritability
Vg = H2 * G.Loci        # additive genotypic variance
Ve = (1-H2) * Vg/H2     # environmental variance
SDe = sqrt(Ve)          # by def of sd
Prop.LW = 0.85          # initial proportion of LW (long wing)
# Set threshold value. Values greater than Z.LW are LW
Z.LW = qnorm(1-Prop.LW, mean=G.Loci, sd=sqrt(Vg+Ve))
# Generate matrix of individuals in which rows hold individuals whitle columns hold loci.
# Allelic values are 1 and 0. Randomly generate Tot.Loci.
D = round(runif(Tot.Loci))
Genotype = matrix(D, N, N.Loci)
Maxgen = 30
Output = matrix(0, Maxgen, 5)
Output[,1]

for(i in 1:Maxgen) {
    E.X = rnorm(N, mean=0, sd=SDe)
    # phenotypic values of liability
    Genotype.Sum = rowSums(Genotype)
    Phenotype = Genotype.Sum + E.X
    Vg = var(Genotype.Sum)
    Vp = var(Phenotype)
    H2 = Vg / Vp
    # calculate wing morphs by comparing liability to threshold
    Morph = matrix(1, N)
    Morph[Phenotype > Z.LW] = 0
    Prop.SW = sum(Morph) / N
    Output[i, 1] = i        # generation number
    Output[i, 2] = Prop.SW  # proportion short wing
    Output[i, 3] = H2       # heritability
    # apply selection; no selection until after gen 10
    if(i < 10) {
        Parents = Genotype
    } else {
        Parents = selection(Morph, Genotype)
    }    
    N.Parents = nrow(Parents) 
    # form next generation; apply mutation
    Lambda = P * N.Parents * N.Loci
    # number of mutations using Poisson distribution
    N.Mutations = rpois(1, Lambda)
    Output[i, 4] = N.Mutations # number in this generation
    # generate mutant loci
    Genotype = mutation(Genotype, N.Mutations, Tot.Loci, N)
    # mating; produce gametes for female offspring; select from each row at random
    # does not distinguish individual loci; creates a matrix of G.Loci rows and N.Loci cols
    # matrix is transposed to needed shape
    Gametes = apply(Parents, 1, gamete, G.Loci, N.Loci)
    Gametes = t(Gametes)
    Output[i, 5] = N.Parents
    # sample N gametes from "females"
    Index = seq(1, N.Parents)
    G.Index = sample(x=Index, size=N, replace=T)
    F.Gametes = Gametes[G.Index, ]
    # sample N gametes from "males"
    G.Index = sample(x=Index, size=N, replace=T)
    M.Gametes = Gametes[G.Index, ]
    # new genotypes
    Genotype = cbind(F.Gametes, M.Gametes)
}    

# graphing parameters
par(mfrow=c(2,2))
plot(Output[,1], Output[,2], type='l', xlab='Generation', ylab='Proportion Short Wings')
plot(Output[,1], Output[,3], type='l', xlab='Generation', ylab='Heritability')
plot(Output[,1], Output[,4], type='l', xlab='Generation', ylab='N Mutations')
plot(Output[,1], Output[,5], type='l', xlab='Generation', ylab='N Parents')

