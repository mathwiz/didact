fun1 = lambda x, a, b : a*x + b

fun2 = lambda x : a*x + b

def fun3(x):
  return a*x + b

print('all arguments passed')
for i in range(1,3):
  for a in range(1,4):
    r = fun1(i, a, 2*a)
    print(r)

print('lambda')
for i in range(1,3):
  for a in range(1,4):
    b = 2*a
    r = fun2(i)
    print(r)

print('def function')
for i in range(1,3):
  for a in range(1,4):
    b = 2*a
    r = fun3(i)
    print(r)

print('stored lambdas')
funs = []
for i in range(1,4):
  a = i
  b = 2*a
  funs.append( lambda x: a * x + b )

print("since dynamic scope, the functions all use current values of a and b")
print("a=", a, "b=", b)
for i in range(1,3):
  for f in funs:
    r = f(i)
    print(r)


