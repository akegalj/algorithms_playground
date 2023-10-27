def solution(A, K):
    s = 0 if not A else K % len(A)
    return (A[-s:] if s else []) + A[:len(A)-s]