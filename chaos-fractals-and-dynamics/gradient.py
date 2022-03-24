import casioplot as scr

def hsv2c(h,s,v):
  c=v*s
  x,m,k=c*(1-abs((h%(2/3))*3-1)),v-c,(h*3)//1
  t1 = round(255*(m+x*(k%3==1)+c*(k%5==0)))
  t2 = round(255*(m+c*(k==1 or k==2)+x*(k%3==0)))
  t3 = round(255*(m+x*(k%3==2)+c*(k==3 or k==4)))
  return (t1, t2, t3)

def grad(x,y,w,h):
  for i in range(w):
    for j in range(h):
      arg1 = 2*j/(h-1)
      arg2 = i>=w//2 and 1 or i/(w//2-1)
      arg3 = i<w//2 and 1 or (w-1-i)/((w-w//2)-1)
      c=hsv2c(arg1, arg2, arg3)
      scr.set_pixel(x+i,y+j,c)

grad(0, 0, 384, 192)
scr.show_screen()
