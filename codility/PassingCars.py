
def solution(A):
    # write your code in Python 2.7
    zeroCount = 0
    totalPairs = 0
    MAX_PAIRS = 10**9
    for x in A:
        if totalPairs > MAX_PAIRS:
            return -1
        elif x == 0:
            zeroCount += 1
        else:
            totalPairs += zeroCount
    return totalPairs

