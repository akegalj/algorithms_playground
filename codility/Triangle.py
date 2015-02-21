def solution(A):
    # write your code in Python 2.7
    sortedA = sorted(A)
    for x in xrange(2,len(A)):
        # The list is sorted, so A[index+i] >= A[index+2]
        # where i>2. If A[index]+A[index+1] <= A[index+2],
        # then A[index]+A[index+1] <= A[index+i], where
        # i>=2. So there is no element in A[index+2:] that
        # could be combined with A[index] and A[index+1]
        # to be a triangular.
        if  sortedA[x-2] + sortedA[x-1] > sortedA[x]:
            return 1
    return 0

