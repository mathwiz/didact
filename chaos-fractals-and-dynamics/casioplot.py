import pygame 
import pygame.gfxdraw

class Screen:
  def __init__(self):
    pygame.init()
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

  def set_pixel(self, x, y, rgb=(0, 0, 0)):
    pygame.draw.circle(self.scr, rgb, (x, y), 1)

  def draw_string(self, x, y, text, rgb=(0, 0, 0), text_size='medium'):
    screen = self.scr
    s = self.s
    pygame.draw.circle(screen, "green", (50, 100), 10)
    pygame.draw.circle(screen, "black", (50, 100), 10, 1)
    pygame.display.flip()



def show_screen():
   _screen.show()

def clear_screen():
   _screen.clear()

def get_pixel(x, y):
  return _screen.get_pixel(x, y)

def set_pixel(x, y, rgb=(0, 0, 0)):
  _screen.set_pixel(x, y, rgb)

def draw_string(x, y, rgb=(0, 0, 0), text_size='medium'):
  _screen.draw_string(x, y, "test", rgb, text_size)


_screen = Screen()
draw_string(20,40)
set_pixel(10, 20, (255,0,0))
show_screen()
