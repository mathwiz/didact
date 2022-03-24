from turtle import *
from math import *

def mypencolor(t):
  cmax = 255
  try:
    pencolor((2, 2, 2))
  except:
    cmax = 1
  if(cmax == 1 and max(t)>1):
    t = tuple(u/255 for u in t)
  elif(cmax == 255 and any(isinstance(u, float) for u in t)):
    t=tuple(int(255*u) for u in t)
  pencolor(t)

penup()
goto(0, -20)
pendown()
for i in range(1,37):
  red=(exp(-0.5 * ((i-6)/12)**2))
  green=(exp(-0.5 * ((i-18)/12)**2))
  blue=(exp(-0.5 * ((i-30)/12)**2))
  mypencolor([red,green,blue])
  circle(50-i)
  right(10)
