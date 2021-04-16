
f = set(map(int, open("input.txt").read().split()))

print({i * (2020 - i) for i in f if 2020 - i in f })

print({ i * j * (2020 - i - j) for i in f for j in f if 2020 - i - j in f})
