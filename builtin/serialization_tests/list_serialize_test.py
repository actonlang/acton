import pickle

def main():
    lst = []
    for i in range(0,30):
        if i%2 != 0:
            lst.append(lst[i//2])
        else:
            lst.append([i*i for i in range(0,i)])
    fileptr = open("pytest2.bin","wb")
    pickle.dump(lst,fileptr)


main()

