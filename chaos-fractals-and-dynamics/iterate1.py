x0 = float(input("x0? "))
for i in range(25):
    x0 = 2*x0*(1-x0)
    print(f'{i + 1} {x0}')

