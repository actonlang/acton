def test():
    d = dict();
    for i in range(1,1000000): d[i] = i+1
    r = 17
    s=0
    for i in range(1,100000):
        r = r*r % 1000000
        s += d[r];
    print("Summed 100000 values; sum is ",s)
    t1 = 678 in d
    t2 = -1 in d
    if (t1 and not t2):
        print("contains test ok")
    else:
        print("contains test failed")
    for i in range(1,1000000):
        if (i%100 > 0): d.pop(i)
    print("size of dictionary after popping is ",len(d))
    t = 0
    for i in iter(d): t += i
    print ("Sum of remaining keys is ",t)
    return
    
test()


        
    
        
