with open("day03.txt", "rt") as f:
	ins = f.read()

n = 0
ll = list()
la = list()

for l in ins.split('\n'):
    ll.append(map(int, l.split()))

for i in range (len(ll)/3):
    for j in range(3):
        ln = [ll[i*3][j], ll[i*3+1][j], ll[i*3+2][j]]
        ln.sort()
        la.append(ln)

for l in la:
    if l[0]+l[1] > l[2]:
        n += 1

print n