fun1 = lambda x, a, b : a*x + b

fun2 = lambda x : a*x + b

def fun3(x):
  return a*x + b

print('take 1')
for i in range(1,3):
  for a in range(1,4):
    r = fun1(i, a, 2*a)
    print(r)

print('take 2')
for i in range(1,3):
  for a in range(1,4):
    b = 2*a
    r = fun2(i)
    print(r)

print('take 3')
for i in range(1,3):
  for a in range(1,4):
    b = 2*a
    r = fun3(i)
    print(r)


