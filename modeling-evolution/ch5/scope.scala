import scala.collection.mutable.ArrayBuffer

val fun1 = (x:Int, a:Int, b:Int) => a*x + b

// no go for this definition
//val badfun3 = (x:Int) => a*x + b

// having a value for a and b should have no effect since local a and b hide them
val a = -100
val b = -200

println("a and b supplied as args")
for(i <- 1 to 2) {
  for(j <- 1 to 3) {
    val a = j
    val b = 2*a
    println( fun1(i, a, b) )
  }
}

println("locally created lambda. could be static or dynamic")
for(i <- 1 to 2) {
  for(j <- 1 to 3) {
    val c = j
    val d = 2*c
    val fun2 = (x:Int) => c*x + d
    println( fun2(i) )
  }
}

val funs: ArrayBuffer[Int => Int] = ArrayBuffer()
println("stored lambda works so static")
for(j <- 1 to 3) {
  val e = j
  val f = 2*e
  funs += ( (x:Int) => e*x + f )
}
// e, f now out of scope
//println(e)
for(i <- 1 to 2) {
  for(f <- funs) {
    println( f(i) )
  }
}


