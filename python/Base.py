def id(x): 
    return x

def flip(f): 
    return lambda x, y: f(y, x)

def const(x, y): 
    return x


if __name__ == "__main__":
    pass