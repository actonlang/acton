def test():
    d = dict();
    for i in range(1,1000000): d[str(i)] = i+1
    r = 17
    s=0
    for i in range(1,100000):
        r = r*r % 1000000
        b = d[str(r)]
        s += b;
    print("in dict_test after summation; last value retrieved should be",r+1,", was ",b)
    print("Summed 100000 values; sum is ",s)
    t1 = "678" in d
    t2 = "-1" in d
    if (t1 and not t2):
        print("contains test ok")
    else:
        print("contains test failed")
    for i in range(1,1000000):
        if (i%100 > 0): res = d.pop(str(i))
    print("Last popped is",res);
    print("Size of dictionary after popping is ",len(d))
    t = 0
    for i in iter(d): t += int(i)
    print ("Sum of remaining keys is ",t)
    deflt = d.get("100",666)
    print("dict_get on existing key 100; should return 101. Returned ",deflt)
    deflt = d.get("37",666)
    print("dict_get on non-existing key; should return default value 666. Returned ",deflt)
    other = dict()
    for j in range(11,200,20):
      other[str(j)] = 2*j
    d.update(other)
    items = iter(d.items())
    for k in range(0,10):
        key,value = next(items)
        print("item #",k,"is: key=",key,", value=",value)
    key,value = d.popitem()
    print("popitem gives: key=",key,", value=",value)
    key,value = d.popitem()
    print("popitem gives: key=",key,", value=",value)
    print("Size of dictionary should be 10007; is ",len(d));
    return
    
test()


        
    
        
