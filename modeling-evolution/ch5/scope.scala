val fun1 = (x:Int, a:Int, b:Int) => a*x + b

//val badfun3 = (x:Int) => a*x + b

println("take 1")
for(i <- 1 to 2) {
  for(a <- 1 to 3) {
    println( fun1(i, a, 2*a) )
  }
}

println("take 2")
for(i <- 1 to 2) {
  for(a <- 1 to 3) {
    val b = 2*a
    val fun2 = (x:Int) => a*x + b
    println( fun2(i) )
  }
}