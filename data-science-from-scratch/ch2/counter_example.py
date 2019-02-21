from collections import Counter


words = """
It was the best of times, it was the worst of times, 
it was the age of wisdom, it was the age of foolishness, 
it was the epoch of belief, 
it was the epoch of incredulity, 
it was the season of light, 
it was the season of darkness, 
it was the spring of hope, 
it was the winter of despair.
""".lower().split()


c = Counter([0,1,2,0])

print(c)

word_counts = Counter(words)

print(word_counts)

print(word_counts.most_common(5))
