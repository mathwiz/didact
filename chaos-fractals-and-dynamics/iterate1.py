x0 = float(input("x0? "))
c = float(input("c? "))
n = int(input("n? "))
for i in range(n):
    x0 = c * x0 * (1-x0)
    print(f'{i + 1} {x0}')

