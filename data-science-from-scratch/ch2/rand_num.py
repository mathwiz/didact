import random


random.seed(323)
uniform_randoms = [random.random() for _ in range(4)]
print(uniform_randoms)


print(random.randrange(10))
print(random.randrange(3,6))


up_to_ten = list(range(10))
random.shuffle(up_to_ten)
print(up_to_ten)


print(random.choice(range(10)))


lottery_numbers = range(69)
print(random.sample(lottery_numbers, 5))


four_with_replacement = [random.choice(range(10)) for _ in range(4)]
print(four_with_replacement)

