sz = 30000000
ages = [0] * sz

starting = list(map(int, open("input.txt").read().strip().split(',')))

def gen(s=starting, end=2020):
  for i in range(len(s)): ages[s[i]] = i
  x = len(s)
  ages[0] = x
  y = ages[x]
  i = x+1
  while i < end-1:
    ages[x] = i
    x = i - y if y != 0 else 0
    y = ages[x]
    i += 1
  return x

from timeit import timeit
print(timeit(
  lambda: print(gen(end=sz)),
  number=1
))
