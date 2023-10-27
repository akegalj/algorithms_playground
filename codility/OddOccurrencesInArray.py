def solution(A):
    prev = 0
    count = 0
    for e in sorted(A):
        if e == prev:
            count += 1
        else:
            if count % 2 == 1:
                return prev
            prev = e
            count = 1
    return prev