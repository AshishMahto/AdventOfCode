sz = 30000000
ages = [-1] * sz

starting = list(map(int, open("input.txt").read().strip().split(',')))

def gen(s=starting, end=2020):
  for i in range(len(s)-1): ages[s[i]] = i
  i = len(s)-1
  x = s[i]
  while i < end-1:
    y = ages[x]
    ages[x] = i
    x = 0 if y == -1 else i - y
    i += 1
  return x

from timeit import timeit
print(timeit(
  lambda: print(gen(end=sz)),
  number=1
))
