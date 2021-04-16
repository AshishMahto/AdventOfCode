
f = open("input.txt").read().strip().split('\n')
import re

r = re.compile(r"(\d+)-(\d+) (\w): (\w+)")

part1 = 0
part2 = 0
for ln in f:
  a, b, c, s = r.match(ln).groups()
  x, y = int(a), int(b)
  part1 += x <= s.count(c) <= y
  part2 += (s[x-1] == c) != (s[y-1] == c)

print(part1)
print(part2)
