from collections import deque
from math import floor


def solve(amount):
    circle = deque(range(amount, 0, -1))
    while len(circle) > 1:
        circle.rotate(1)
        circle.pop()
    return circle.pop()


def solve2(amount):
    circle = deque(range(amount, 0, -1))
    while len(circle) > 1:
        circle.rotate(floor(len(circle) / 2))
        circle.pop()
        circle.rotate(floor(len(circle) / 2) + 1)
        if len(circle) % 10000 == 0:
            print(len(circle))
    return circle.pop()


print(f"PART 1: {solve(3018458)}")
print(f"PART 2: {solve2(3018458)}")
