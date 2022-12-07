p = 20_201_227

with open("input.txt", 'r') as f:
  card_pub, door_pub = map(int, f.read().strip().split('\n'))

# card_pub, door_pub = 5764801, 17807724

print(card_pub, door_pub)

def go(n=p):
  enc = 1
  for secret in range(n):
    if card_pub == enc: return door_pub, secret
    elif door_pub == enc: return card_pub, secret
    enc = (enc * 7) % p
pub, sec = go()
print(f"part1: {pow(pub, sec, p) = }")
