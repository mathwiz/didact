from casioplot import *
from random import randint

x = [180, 319, 41]
y = [0, 192, 192]
xn = 180
yn = 0

for i in range(1, 8*10**3):
  b = randint(0, 2)
  xn = (xn + x[b])//2
  yn = (yn + y[b])//2
  coffset = i % 255
  color = (255-coffset, 0, coffset)
  set_pixel(xn, yn, color)
  show_screen()
