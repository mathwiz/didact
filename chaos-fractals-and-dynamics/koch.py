from turtle import *

def koch(n, l):
  if n==0:
    forward(l)
  else:
    koch(n-1, l/3)
    left(60)
    koch(n-1, l/3)
    right(120)
    koch(n-1, l/3)
    left(60)
    koch(n-1, l/3)

pencolor("blue")
penup()
goto(-180, -50)
pendown()
koch(4, 360)
