
def hs(n):
    c = 0;
    while n != 1:
        if n % 2 == 0:
            n = n/2;
            c = c + 1;
            print (n);
        else:
            n = 3 * n + 1;
            c = c + 1;
            print (n);
    if n == 1:
        c = c + 1;
        print (c);
    
    return None;
        
        
        
        
