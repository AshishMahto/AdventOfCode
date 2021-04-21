
import itertools as it
from collections import deque

inp = map(int, open("input.txt").read().strip().split())
preamble_ls = list(it.islice(inp, None, 25))
preamble1 = deque(preamble_ls)
preamble2 = set(preamble_ls)
ls = list(inp)

def go():
  for i in ls:
    def exists():
      for j in preamble1:
        k = i-j
        if k != i and k in preamble2: return k
      return None
    k = exists()
    if k:
      d = preamble1.popleft()
      preamble2.remove(d)
      preamble1.append(i)
      preamble2.add(i)
    else: return i

target = go()
print(f"part1: {target}")

import itertools as it
ls = preamble_ls + ls
acc = list(it.accumulate(ls))

print(
  *[
    (min(ls[i:j]) + max(ls[i:j]), ls[i:j])
    for i in range(1, len(acc))
    for j in range(i+2, len(acc))
    if acc[j-1] - acc[i-1] == target
  ],
  sep="\n"
)
