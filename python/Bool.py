from Base import flip, const

# values: true, false

bTrue = const 
bFalse = flip(const)

def boolEval(b):
    """
    To make things easier, evaluate some bool to be true or false
    """
    return b("True", "False")

# operations

# not

bNot = flip

# and, or, xor

def bAnd(b1, b2):
    return b1(b2, bFalse)

def bOr(b1, b2):
    return b1(bTrue, b2)

def bXor(b1, b2):
    return b1(bNot(b2), b2)


if __name__ == "__main__":
    for f in (bNot, ):
        print(f.__name__)
        for b1 in (bTrue, bFalse):
            print(boolEval(b1), boolEval(f(b1)))
    for f in (bAnd, bOr, bXor):
        print(f.__name__)
        for b1 in (bTrue, bFalse):
            for b2 in (bTrue, bFalse):
                print(boolEval(b1), boolEval(b2), boolEval(f(b1, b2)))
