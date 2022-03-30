import casioplot as scr

def plot(x, y, c):
  scr.set_pixel(x, y, c)

for i in range(384):
  for j in range(192):
    if i % 7 == 0 and j % 5 == 0:
      plot(i, j, (i % 255, j % 255, (i+j) % 255))

scr.show_screen()


