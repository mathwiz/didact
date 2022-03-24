import pygame 
import pygame.gfxdraw

class Screen:
  def __init__(self):
    pygame.init()
    self.colormap = {}
    self.width = 384
    self.height = 192
    self.scr = pygame.display.set_mode((self.width, self.height))
    self.clear()

  def is_on(self, x, y):
    return (x > 0 and x < self.width) and (y > 0 and y < self.height)

  def show(self):
    try:
      while True:
        event = pygame.event.wait()
        if event.type == pygame.QUIT:
          break
        if event.type == pygame.KEYDOWN:
          if event.key == pygame.K_ESCAPE or event.unicode == "q":
            break
        pygame.display.flip()
    finally:
      pygame.quit()

  def clear(self):
    self.scr.fill((255,255,255))
    self.s = pygame.Surface(self.scr.get_size(), pygame.SRCALPHA, 32)
    pygame.display.update()

  def set_pixel(self, x, y, rgb=(0, 0, 0)):
    self.colormap[(x, y)] = rgb
    pygame.draw.circle(self.scr, rgb, (x, y), 1)
    pygame.display.update()

  def get_pixel(self, x, y):
    return self.colormap.get((x,y), (255, 255, 255))

  def draw_string(self, x, y, text, rgb=(0, 0, 0), text_size='medium'):
    fontSize = 24 #todo use text_size
    pygame.font.init()
    myfont = pygame.font.SysFont('Helvetica', fontSize)
    textsurface = myfont.render(text, False, rgb)

    self.scr.blit(textsurface, (x, y))
    pygame.display.update()



def show_screen():
   _screen.show()

def clear_screen():
   _screen.clear()

def get_pixel(x, y):
  return _screen.get_pixel(x, y)

def set_pixel(x, y, rgb=(0, 0, 0)):
  _screen.set_pixel(x, y, rgb)

def draw_string(x, y, text, rgb=(0, 0, 0), text_size='medium'):
  _screen.draw_string(x, y, text, rgb, text_size)


_screen = Screen()
draw_string(200, 100, "Hello World!", (100, 89, 200))
set_pixel(10, 20, (255,0,0))
set_pixel(110, 90, (0,255,0))
set_pixel(210, 180, (0,0,255))
print(get_pixel(10, 20))
show_screen()
