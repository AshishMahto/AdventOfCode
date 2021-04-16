f = [
  s.split()
  for s in open("input.txt").read().strip().split('\n\n')
]

print(sum(
  len(set.union(*map(set, grp)))
  for grp in f
))

print(sum(
  len(set.intersection(*map(set, grp)))
  for grp in f
))
