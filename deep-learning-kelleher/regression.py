import numpy as np
import statsmodels.api as sm
import pandas as pd
import examples



# statsmodels
iris = examples.iris
df = pd.DataFrame(iris.data, columns=iris.feature_names)
y = df['petal width (cm)']
x = df['petal length (cm)']
x = sm.add_constant(x) # add intercept to model

fit = sm.OLS(y, x).fit()
print(fit.summary())

