with open('input.txt', 'r') as open_file:
    locs = [l.strip() for l in open_file.readlines()]
    locs = [tuple([int(c) for c in l.split(',')]) for l in locs]

def norm(a, b):
    return ((a[0] - b[0]) ** 2) + ((a[1] - b[1]) ** 2) + ((a[2] - b[2]) ** 2)

inMST = [False for _ in locs]
parent = [None for _ in locs]
key = [float('inf') for _ in locs]

key[0] = 0

for _ in locs:
    u = None
    minKey = float('inf')

    for i in range(len(locs)):
        if not inMST[i] and key[i] < minKey:
            u = i
            minKey = key[i]
    
    inMST[u] = True

    for i in range(len(locs)):
        if not inMST[i] and norm(locs[u], locs[i]) < key[i]:
            key[i] = norm(locs[u], locs[i])
            parent[i] = locs[u]

maxOut = None
maxNorm = 0
for j in range(1, len(locs)):
    dist = norm(locs[j], parent[j])

    if dist > maxNorm:
        maxNorm = dist
        maxOut = locs[j][0] * parent[j][0]

print(maxOut)