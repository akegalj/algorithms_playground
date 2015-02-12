def solution(A):
    # write your code in Python 2.7
    right = sum(A)
    left = 0
    best = 10**8
    for x in xrange(len(A) - 1):
        left += A[x]
        right -= A[x]
        best = min(abs(left - right), best)
    return best

