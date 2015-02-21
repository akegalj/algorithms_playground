def solution(S, P, Q):
    # write your code in Python 2.7
    impactFactors = [(0,0,0,0)]
    for x in xrange(len(S)):
        t = impactFactors[x]
        if S[x] == 'A':
            impactFactors.append((t[0]+1,t[1],t[2],t[3]))
        elif S[x] == 'C':
            impactFactors.append((t[0],t[1]+1,t[2],t[3]))
        elif S[x] == 'G':
            impactFactors.append((t[0],t[1],t[2]+1,t[3]))
        elif S[x] == 'T':
            impactFactors.append((t[0],t[1],t[2],t[3]+1))
    minimalImpact = []
    for x in xrange(len(P)):
        for y in xrange(4):
            factor = impactFactors[Q[x]+1][y] - impactFactors[P[x]][y]
            if factor > 0:
                minimalImpact.append(y+1)
                break
    return minimalImpact

