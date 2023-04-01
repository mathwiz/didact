rm(list=ls())


#### Main program ####
A.0 = B.xy = B.yx = 0.8
A.xy = C.xy = C.yx = 0.4

N = 100
X = seq(from=0.0, to=3.0, length=N)
S.xy = -B.xy*X + C.xy*X^2

print('two-traits done.')

output = function() {
    par(mfcol=c(2,1))
    plot(X, S.xy, type='l', xlab='Vigilance or Foraging Rate', ylab='Effect on Survival', las=1, lwd=3)
    ## print("Fitness by numeric integration (nlm, optimize)")
    ## print(c(Max.Root.Num1, Max.Root.Num2))
}    
