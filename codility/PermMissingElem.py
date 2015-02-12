def solution(A):
    # write your code in Python 2.7
    K = len(A) + 1
    return K*(K+1)/2 - sum(A)

