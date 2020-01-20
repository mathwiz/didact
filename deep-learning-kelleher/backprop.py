import numpy as np
import pandas as pd
import examples


class Network:

    def __init__(self, layer_sizes):
        self.layers = make_network(layer_sizes)
        self.sums = []
        self.activations = []

    def show(self):
        return self.layers
        
    def run(self):
        # store the forward pass values for z(k) a(k)
        self.sums = []
        self.activations = []


def relu(val):
    return val if val > 0 else 0
    
    
def identity(val):
    return val


def produce_value(val, size=1):
    return np.repeat(val, size)


def sum(input, weights):
    return np.sum(input * weights)
    

def initial_weights(n, max=1):
    return np.random.uniform(low=0, high=max, size=n)    


def activation(val, func=relu):
    return func(val)


def make_layer(size, max=1):
    return initial_weights(size, max)
    

def make_io_layer(size):
    return produce_value(1, size)
    
    
def make_network(sizes):
    f = lambda size, io: make_io_layer(size) if io else make_layer(size)
    return [f(s, i==1 or i==len(sizes)) for i,s in enumerate(sizes, 1)]
    

# initialize
net = Network([1,3,2,1])

