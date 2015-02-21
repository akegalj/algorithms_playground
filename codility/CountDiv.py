import math

def solution(A, B, K):
    # write your code in Python 2.7
    first = int(math.ceil(A / float(K)))*K
    if first > B:
        return 0
    return (B - first) / K + 1

