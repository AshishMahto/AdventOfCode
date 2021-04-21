
from Advent2019.intcode import IntCode

ic = IntCode()
print(ic.instrs)

print(ic.run((1, 12), (2, 2)))
