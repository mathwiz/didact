import numpy as np
import pandas as pd
import examples


class Network:

    def __init__(self, layer_sizes):
        self.layers = layer_sizes

    def run(self):
        return "nothing yet"


def relu(val):
    return val if val > 0 else 0
    
    
def identity(val):
    return val


def produce_output(val, size=1):
    return np.repeat(val, size)


def sum(input, weights):
    return np.sum(input * weights)
    

def initial_weights(n, max=1):
    return np.random.uniform(low=0, high=max, size=n)    


# initialize

