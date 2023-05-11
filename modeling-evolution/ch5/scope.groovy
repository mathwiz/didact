def fun1 = { x, a, b -> a*x + b }

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

println("dynamic does not run. just show a")
for (i in 1..2) {
  for (a in 1..3) {
    // does not work for fun2 or fun3
    def r = a  //fun3(i)
    println(r)
  }
}

println("static binding when lambda created?")
for (i in 1..2) {
    for (a in 1..3) {
        def b = 2*a
        def fun2 = { x -> a*x + b }
        def r = fun2(i)
        println(r)
    }
}

println("stored lambda works so static scope is true")
for (i in 1..3) {
    def a = i
    def b = 2*a
    funs.add( { x -> a * x + b } )
}

for (i in 1..2) {
    for (f in funs) {
        println(f(i))
    }
}



