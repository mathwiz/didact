f<- function(x) { 2/x + 2*x + 1/sqrt(x) }
integrate(f, 1, 4)

                                        # Improper integrals

integrate(function(x) { 1/(x-1)^2 }, 2, Inf)

integrate(function(x) { 1/(x-1)^(1/2) }, 2, Inf) # divergent

integrate(function(x) { 1/(x-1)^(1/2) }, 1, 2)

integrate(function(x) { 1/(x-1)^2 }, 1, 2) # divergent


