def solution(A):
    # write your code in Python 2.7
    bucket = [False] * len(A)
    for x in A:
        if x <= len(A) and x > 0:
            bucket[x-1] = True
    for x in xrange(len(A)):
        if bucket[x] == False:
            return x+1;
    return len(A) + 1

