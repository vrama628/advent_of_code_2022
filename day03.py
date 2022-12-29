def priority(c):
    return ord(c) - 96 if ord(c) > 90 else ord(c) - 38

total = 0
try:
    while True:
        total += priority(set(input()).intersection(input()).intersection(input()).pop())
except EOFError:
    print(total)