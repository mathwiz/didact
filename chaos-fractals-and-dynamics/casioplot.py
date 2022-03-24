from PIL import Image, ImageDraw

class Screen:
    def __init__(self):
        self.load_image("empty.png")
        self.width = 384
        self.height = 192

    def load_image(self, file):
        self.image = Image.open(file)

    def is_on(self, x, y):
        return (x > 0 and x < 300) and (y > 0 and y < 150)

    def show(self):
        self.image.show()

    def clear(self):
        self.load_image("empty.png")
        self.show()

    def set_pixel(self, x, y):
        self.image.show()

    def set_pixel(self, x, y, rgb=(0, 0, 0)):
        self.image.show()

    def draw_string(self, x, y, text, rgb=(0, 0, 0), text_size='medium'):
        with self.image as im:
            draw = ImageDraw.Draw(im)
            draw.line((0,0) + im.size, fill=128)
            draw.line((0, im.size[1], im.size[0], 0), fill=200)



_screen = Screen()

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
