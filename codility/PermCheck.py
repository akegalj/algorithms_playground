def solution(A):
    # write your code in Python 2.7
    unique = set(A)
    if len(unique) == len(A) and min(unique) == 1 and max(unique) == len(A):
        return 1
    return 0

