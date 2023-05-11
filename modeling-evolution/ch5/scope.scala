import scala.collection.mutable.ArrayBuffer

val fun1 = (x:Int, a:Int, b:Int) => a*x + b

// no go for this definition
//val badfun3 = (x:Int) => a*x + b

println("a and b supplied as args")
for(i <- 1 to 2) {
  for(a <- 1 to 3) {
    println( fun1(i, a, 2*a) )
  }
}

println("locally created lambda. could be static or dynamic")
for(i <- 1 to 2) {
  for(a <- 1 to 3) {
    val b = 2*a
    val fun2 = (x:Int) => a*x + b
    println( fun2(i) )
  }
}

val funs: ArrayBuffer[Int => Int] = ArrayBuffer()
println("stored lambda works so static")
for(a <- 1 to 3) {
  val b = 2*a
  funs += ( (x:Int) => a*x + b )
}
// a, b now out of scope
//println(b)
for(i <- 1 to 2) {
  for(f <- funs) {
    println( f(i) )
  }
}
