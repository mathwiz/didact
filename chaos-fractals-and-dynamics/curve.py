from matplotl import *

def f(a):
  return 6*a-0.1*a**2
x=list(range(70))
y=[f(i) for i in x]
plot(x,y)
show()
