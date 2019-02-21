from collections import defaultdict

word_counts = defaultdict(int) # int() produces 0

words = """
It was the best of times, it was the worst of times, 
it was the age of wisdom, it was the age of foolishness, 
it was the epoch of belief, 
it was the epoch of incredulity, 
it was the season of light, 
it was the season of darkness, 
it was the spring of hope, 
it was the winter of despair.
""".split()

for word in words:
    word_counts[word] += 1


print(word_counts)

