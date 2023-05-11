fun1 = function(x, a, b) { a*x + b }

fun2 = function(x) { a*x + b }

print('all arguments passed')
for(i in 1:2) {
    for(a in 1:3) {
        r = fun1(i, a, 2*a)
        print(r)
    }
}    

print('dynamic scope')
for(i in 1:2) {
    for(a in 1:3) {
        b = 2*a
        r = fun2(i)
        print(r)
    }
}    
