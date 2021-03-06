from functools import partial, reduce


def exp(base, power):
    return base ** power


two_to_the = partial(exp, 2)
print(two_to_the(3))


square_of = partial(exp, power=2)
print(square_of(3))


xs = [1,2,3,4]


print([two_to_the(x) for x in xs])
powers_of_two = map(two_to_the, xs)
print(list(powers_of_two))
list_powers_of_two = partial(map, two_to_the)
print(list(list_powers_of_two(xs)))


print([x for x in xs if x % 2 == 0])
print(list(filter(lambda x: x % 2 == 0, xs)))
list_evener = partial(filter, lambda x: x % 2 == 0)
print(list(list_evener(xs)))


print(reduce(lambda x, y: x * y, xs))
list_product = partial(reduce, lambda x, y: x * y)
print(list_product(xs))


print(reduce(lambda x, y: x + square_of(y), xs))
list_sum_of_squares = partial(reduce, lambda x, y: x + square_of(y))
print(list_sum_of_squares(xs))
