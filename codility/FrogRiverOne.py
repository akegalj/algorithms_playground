def solution(X, A):
    # write your code in Python 2.7
    bucket = set()
    for x in xrange(len(A)):
        bucket.add(A[x])
        if len(bucket) == X and min(bucket) == 1 and max(bucket) == X:
            return x
    return -1

