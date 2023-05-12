fun1 = function(x, a, b) { a*x + b }

fun2 = function(x) { a*x + b }

print('calling function defined earlier with all arguments passed')
for(i in 1:2) {
    for(j in 1:3) {
        a = j
        b = 2*a
        print( fun1(i, a, b) )
    }
}    

print('calling function defined earlier with unbound a and b')
for(i in 1:2) {
    for(j in 1:3) {
        a = j
        b = 2*a
        print( fun2(i) )
    }
}    

print('calling function defined locally')
for(i in 1:2) {
    for(j in 1:3) {
        c = j
        d = 2*c
        fun = function(x) { c*x + d }
        print( fun(i) )
    }
}    

print('call functions created with a and b in context; functions are stored for later call')
funs = list()
for(i in 1:3) {
    e = i
    f = 2*e
    funs[[i]] = function(x) { e*x + f }
}

print(c('during call the values of e and f are: ', e, f))
for(i in 1:2) {
    for(j in 1:3) {
        r = funs[[j]](i)
        print(r)
    }
}    
