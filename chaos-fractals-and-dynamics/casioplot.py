from PIL import Image

class Screen:
    def __init__(self):
        self.load_image("empty.png")

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

    def draw_string(self, x, y, rgb=(0, 0, 0), text_size='medium'):
        self.image.show()


TheScreen = Screen()

def show_screen():
    TheScreen.show()

def clear_screen():
    TheScreen.clear()

def get_pixel(x, y):
    None

