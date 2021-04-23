
from math import inf, lcm

def toInt(s):
  try: return int(s)
  except: return s

t, bus = open("input.txt").read().strip().split()
t = int(t)
bus = list(map(toInt, bus.split(',')))

def get_time(bus_id):
  if bus_id == 'x': return inf
  return -t % bus_id

win = min(
  bus,
  key=get_time
)

print(win, get_time(win), win * get_time(win))

t = 0
m = 1
for i, d in enumerate(bus):
  if d == 'x': continue
  k = ((-i - t) * pow(m, -1, d)) % d
  t += k * m
  m = lcm(m, d)

print(t)
