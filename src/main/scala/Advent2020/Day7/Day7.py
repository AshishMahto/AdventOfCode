import functools


def remove_bags(s):
  if ' bags' in s:
    return s[:-5]
  else:
    return s[:-4]


def toInt(s):
  i, x = s.split(' ', 1)
  return int(i), remove_bags(x)


def parse1(_, x):
  x = x.strip('.').split(', ')
  return remove_bags(_), [toInt(c) for c in x if c != "no other bags"]


f = dict(
  parse1(*ln.split(' contain '))
  for ln in open("input.txt").read().strip().split('\n')
)


@functools.cache
def num_shiny_gold(node): return sum(
  num if child == 'shiny gold' else num_shiny_gold(child)
  for num, child in f[node]
)


@functools.cache
def num_children(node): return sum(
  num + num * num_children(child)
  for num, child in f[node]
)


# print(*f.items(), sep="\n")
print(sum([num_shiny_gold(nd) >= 1 for nd in f]))
print(num_children('shiny gold'))
