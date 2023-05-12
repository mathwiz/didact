fun1(x, a, b) = a*x + b

println("unbound a, b allowed in definition")
fun2(x) = a*x + b

function test_all_args()
    for i in 1:2
        for j in 1:3
            a = j
            b = 2 * a
            println( fun1(i, a , b) )
        end    
    end
end    

function test_unbound_args()
    for i in 1:2
        for j in 1:3
            a = j
            b = 2 * a
            println("fails due to unbound a")
            println( fun2(i) )
        end    
    end
end    

function test_stored_lambdas()
    funs = []
    for j in 1:3
        a = j
        b = 2 * a
        push!(funs, x -> a*x + b )
    end    

    println("captures values for a and b in functions; those values are not passed to function")
    for i in 1:2
        for j in 1:3
            fun = funs[j]
            println( fun(i) )
        end    
    end
end    
