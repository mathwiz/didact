let print = console.log

let fun1 = (x, a, b) => a*x + b 

let fun2 = (x) => a*x + b

print('take 1')
for(let i=1; i<=2; i++) {
    for(let j=1; j<=3; j++) {
        a = j
        b = 2*a
        r = fun1(i, a, b)
        print(r)
    }
}    

print('dynamic on defined function')
for(let i=1; i<=2; i++) {
    for(let j=1; j<=3; j++) {
        a = j
        b = 2*a
        r = fun2(i)
        print(r)
    }
}    


print('dynamic on anonymous function')
for(let i=1; i<=2; i++) {
    for(let j=1; j<=3; j++) {
        a = j
        b = 2*a
        r = ( (x) => a*x + b )(i)
        print(r)
    }
}    

print('stored functions still use dynamically scoped values of a and b')
let funs = []
for(let j=1; j<=3; j++) {
    a = j
    b = 2*a
    funs.push( (x) => a*x + b )
}

for(let i=1; i<=2; i++) {
    funs.forEach( (f) => print( f(i) ) )
}
