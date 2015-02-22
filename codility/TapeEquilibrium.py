def solution(A):
    # write your code in Python 2.7
    right = sum(A)
    left = 0
    best = 10**8
    for x in A[:-1]:
        left += x
        right -= x
        best = min(abs(left - right), best)
    return best

