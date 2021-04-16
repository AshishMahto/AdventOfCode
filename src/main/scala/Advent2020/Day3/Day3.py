from collections import defaultdict

import numpy

f = open("input.txt").read().strip().split('\n')

n = len(f[0])

slopes = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]
posns = [(0,0)] * 5
trees = defaultdict(int)
for i,ln in enumerate(f):
  def update_posn(x, y, a, b):
    if y == i:
      trees[a,b] += ln[x] == '#'
      return (x+a) % n,y+b
    return x,y
  posns = [update_posn(x,y,a,b) for (x,y),(a,b) in zip(posns, slopes)]

print(trees)
print(numpy.prod(list(trees.values())))
