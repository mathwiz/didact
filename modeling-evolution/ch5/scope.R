fun1 = function(x, a, b) { a*x + b }

fun2 = function(x) { a*x + b }

print('calling function defined earlier with all arguments passed')
for(i in 1:2) {
    for(j in 1:3) {
        a = j
        b = 2*a
        r = fun1(i, a, b)
        print(r)
    }
}    

print('calling function defined earlier with unbound a and b')
for(i in 1:2) {
    for(j in 1:3) {
        a = j
        b = 2*a
        r = fun2(i)
        print(r)
    }
}    

print('call functions created with a and b in context; functions are stored for later call')
funs = list()
for(i in 1:3) {
    a = i
    b = 2*a
    funs[[i]] = function(x) { a*x + b }
}

print(c('during call the values of a and b are: ', a, b))
for(i in 1:2) {
    for(j in 1:3) {
        r = funs[[j]](i)
        print(r)
    }
}    
