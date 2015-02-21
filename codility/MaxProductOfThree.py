def solution(A):
    # write your code in Python 2.7
    sl = sorted(A)
    return max(sl[0]*sl[1]*sl[-1], sl[-1]*sl[-2]*sl[-3])

