def gcd(x, y):
    while(y != 0):
        temp = x
        x = y 
        y = temp % y
    return x

print(gcd(40, 56))

