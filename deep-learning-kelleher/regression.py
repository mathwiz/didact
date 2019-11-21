import numpy as np
import statsmodels.api as sm
import pandas as pd
import examples


class Network:

    def __init__(self, layer_sizes):
        self.layers = layer_sizes

    def run(self):
        pass


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


# statsmodels
iris = examples.iris
df = pd.DataFrame(iris.data, columns=iris.feature_names)
y = df['petal width (cm)']
x = df['petal length (cm)']
x = sm.add_constant(x) # add intercept to model

fit = sm.OLS(y, x).fit()
print(fit.summary())

