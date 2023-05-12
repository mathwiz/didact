let print = console.log

let fun1 = (x, a, b) => a*x + b 

let fun2 = (x) => a*x + b

print('all arguments supplied')
for(let i=1; i<=2; i++) {
    for(let j=1; j<=3; j++) {
        z = j
        y = 2*z
        print( fun1(i, z, y) )
    }
}    

print('dynamic on defined function')
for(let i=1; i<=2; i++) {
    for(let j=1; j<=3; j++) {
        a = j
        b = 2*a
        print( fun2(i) )
    }
}    

print('dynamic on anonymous function')
for(let i=1; i<=2; i++) {
    for(let j=1; j<=3; j++) {
        c = j
        d = 2*c
        fun = (x) => c*x + d
        print( fun(i) )
    }
}    

print('stored functions still use dynamically scoped values of a and b')
let funs = [ ]
for(let j=1; j<=3; j++) {
    e = j
    f = 2*e
    funs.push( (x) => e*x + f ) // not 2*x + 4
}

for(let i=1; i<=2; i++) {
    funs.forEach( (f) => print( f(i) ) )
}
