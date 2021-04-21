import functools
from collections import Counter
from operator import sub, mul

ls = list(map(int, open("input.txt").read().strip().split()))
ls.append(0)
ls.append(max(ls) + 3)
ls.sort()
freq = Counter(map(sub, ls[1:], ls))
print(freq, mul(*freq.values()))

n = len(ls)

@functools.cache
def ways(i):
  if i == n-1: return 1
  return sum([ways(j) for j in range(i+1,i+4) if j < len(ls) and ls[j] <= ls[i]+3])

print(ways(0))
