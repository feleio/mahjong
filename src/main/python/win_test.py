def is_win1(h):
    return any(_is_win1(h))

def _is_win1(h):
    if not h:
        yield True
    elif h[0] == 0:
        yield from _is_win1(h[1:])
    else:
        if len(h) >= 3 and h[0] > 0 and h[1] > 0 and h[2] > 0:
            nh = h[:]
            nh[0] -= 1
            nh[1] -= 1
            nh[2] -= 1
            yield from _is_win1(nh)
        if h[0] >= 3:
            nh = h[:]
            nh[0] -= 3
            yield from _is_win1(nh)



def is_win2(h):
    for i in range(9):
        while h[i] > 0:
            if h[i] >= 3:
                h[i] -= 3
            elif i+2 < 9 and h[i] > 0 and h[i+1] > 0 and h[i+2] > 0:
                h[i] -= 1
                h[i + 1] -= 1
                h[i + 2] -= 1
            else:
                return False
    return True

import random
import itertools
for _ in itertools.cycle([0]):
    h=[0]*9
    for j in range(12):
        r = random.randint(0,8)
        while h[r] >= 4:
            r = random.randint(0, 8)
        h[r]+=1
    if is_win1(h[:]) != is_win2(h[:]):
        print(is_win1(h[:]),is_win2(h[:]),h)
        break
