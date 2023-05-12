def fun1 = { x, a, b -> a*x + b }

def fun2 = { x -> a*x + b }

def fun3(x) {
  return a*x + b
}

def funs = []

println("all args")
for (i in 1..2) {
  for (a in 1..3) {
    def r = fun1(i, a, 2*a)
    println(r)
  }
}

println("dynamic test")
for (i in 1..2) {
  for (j in 1..3) {
    def a = j
    def b = 2*a
    // what are a and b?
    println("a = " + a + " b = " + b)
    def r = a*i + b // both fun2 and fun3 crash hard
    println(r)
  }
}

println("static binding must occur when lambdas are created")
for (i in 1..2) {
    for (a in 1..3) {
        def b = 2*a
        def fun = { x -> a*x + b }
        def r = fun(i)
        println(r)
    }
}

println("stored lambda works so static scope is true")
for (i in 1..3) {
    def e = i
    def f = 2*e
    funs.add( { x -> e * x + f } ) // when i = 2, then  2*x + 4
}

for (i in 1..2) {
    for (f in funs) {
        println(f(i))
    }
}



