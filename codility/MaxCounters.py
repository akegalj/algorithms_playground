def solution(N, A):
    # write your code in Python 2.7
    bucket = [0] * N
    top = 0
    lastMC = 0
    for x in xrange(len(A)-1,-1,-1):
        if A[x] == N + 1:
            lastMC = x
            break
    lastMax = 0
    tempMax = 0
    for x in xrange(len(A)):
        if A[x] == N + 1:
            lastMax = tempMax
            if x == lastMC:
                # all containers set to max value
                for y in xrange(N):
                    bucket[y] = tempMax
        else:
            if bucket[A[x]-1] >= lastMax:
                bucket[A[x]-1] += 1
            else:
                bucket[A[x]-1] = lastMax + 1
            tempMax = max(bucket[A[x]-1], tempMax)
    return bucket

