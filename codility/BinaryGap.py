def solution(N):
    maximum = 0
    current = 0
    for x in bin(N)[2:]:
        if x == '1':
            maximum = max(maximum, current)
            current = 0
        else:
            current += 1
    return maximum