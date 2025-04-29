inp = """3   4
4   3
2   5
1   3
3   9
3   3"""

lines = [[int(s) for s in ln.split()] for ln in inp.splitlines()]
l, r = zip(*lines)
l = sorted(l)
r = sorted(r)

print(sum(abs(x - y) for x,y in zip(l, r)))
