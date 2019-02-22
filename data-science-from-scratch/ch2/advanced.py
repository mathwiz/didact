x = [4,1,2,3,5,-2,-1]
y = sorted(x, key=abs, reverse=True)
print(x)
print(y)
x.sort(reverse=True)
print(x)

print([x for x in range(5) if x % 2 == 0])
print([x*x for x in range(5)])
print({x: x*x for x in range(5)})
print([x*x for x in range(-4,4)])
print({x*x for x in range(-4,4)})

print([0 for _ in range(10)])

pairs = [(x,y)
         for x in range(4)
         for y in range(3)]
print(pairs)


def natural_numbers():
    n = 1
    while True:
        yield n
        n += 1


print("Natural numbers below 4 from a lazy sequence")
for it in natural_numbers():
    if it > 4:
        break
    else:
        print(it)
