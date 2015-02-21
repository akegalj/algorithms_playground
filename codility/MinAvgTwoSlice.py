def solution(A):
    # write your code in Python 2.7
    minAvg = 10001
    left = 100000
    for x in xrange(len(A) - 1, 0, -1):
        newAvg = (A[x] + A[x - 1]) / 2.0
        if newAvg <= minAvg:
            minAvg = newAvg
            left = x - 1
        if x == 1:
            continue
        newAvg = (A[x] + A[x - 1] + A[x - 2]) / 3.0
        if newAvg <= minAvg:
            minAvg = newAvg
            left = x - 2
    return left

