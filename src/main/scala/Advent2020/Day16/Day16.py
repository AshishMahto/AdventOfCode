
def parse1(s: str):
  def parseRange(t: str):
    x,y = t.split('-')
    return int(x), int(y)
  k, v = s.split(': ')
  l, r = v.split(' or ')
  return k, (parseRange(l), parseRange(r))

inp = open("input.txt").read()

rules, inp = inp.split("your ticket:")
rules = dict(map(parse1, rules.strip().split('\n')))

my_ticket, other_tickets = inp.split("nearby tickets:")
my_ticket = list(map(int, my_ticket.strip().split(',')))
other_tickets = [list(map(int, ln.split(','))) for ln in other_tickets.strip().split()]


def in_range(i, ls):
  (lx, ly), (rx, ry) = ls
  return lx <= i <= ly or rx <= i <= ry

# return a list of all numbers from `ticket` that violate all rules
def invalid(ticket):
  return tuple(
    k
    for k in ticket
    if all(not in_range(k, r) for _, r in rules.items())
  )

valid_tickets = [t for t in other_tickets if not invalid(t)]
print(f"part1: {sum([x[0] for x in map(invalid, other_tickets) if x])}")

def check_column(col):
  return {k for k, rng in rules.items() if all(in_range(c, rng) for c in col)}

transpose = list(zip(my_ticket, *valid_tickets))
ls = [(i,check_column(col)) for i,col in enumerate(transpose)]

def shake():
  used = set()
  ret = [''] * len(ls)
  prod = 1
  for i,vs in sorted(ls, key=lambda kv: len(kv[1])):
    v = vs - used
    used = vs
    assert len(v) == 1
    key = list(v)[0]
    ret[i] = key
    if "departure" in key: prod *= my_ticket[i]
  return ', '.join(ret), prod

print(*shake(), sep="\npart2: ")
