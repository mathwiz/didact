x = 4
print("x =", x)
print("x is", "even" if x % 2 == 0 else "odd")

for i in range(x):
    print(i, "is less than", x)


y = None
print("y =", y)
print("Is y None?", y is None)

falsy = [False, True, None, [], {}, "", " ", set(), 0, 0.0, -9]

for it in falsy:
    print(f"'{it}': is", "truthy" if it else "falsy")


strings = ["not null", ""]
for s in strings:
    print(f"The first character of '{s}'", "is", s and s[0])


numbers = [None, 0, 1]
for it in numbers:
    num = it or -1
    print("The number for", it, "is", num)
