from math import exp
from turtle import *
for i in range(1,37):
  red=(exp(-0.5 * ((i-6)/12)**2))
  green=(exp(-0.5 * ((i-18)/12)**2))
  blue=(exp(-0.5 * ((i-30)/12)**2))
  pencolor([red, green, blue])
  for i in range(1, 5):
    forward(60)
    right(90)
    right(10)
