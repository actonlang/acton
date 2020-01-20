def concat(ss,zero):
    res = zero
    for i in ss:
        res += i
    return res

def main():
    lst = []
    for i in range(1,10):
        lst.append(range(i,2*i))
    print(concat(lst,[]))

    print(concat(range(1,100),0))

    print(concat("Complicated identity function",""))

main()

    
