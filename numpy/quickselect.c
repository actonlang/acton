// Code closely following Mark Weiss' code at
// http://users.cs.fiu.edu/~weiss/dsaa_c++4/code/Sort.h

#define $SWAP(a,b) {union $Bytes8 tmp = a; a = b; b = tmp;}

static void insertionsort(union $Bytes8 *a, int left, int right, bool (*lt)(union $Bytes8,union $Bytes8)) {
  for (int p = left + 1; p <= right; p++ ) {
    union $Bytes8 tmp = a[p];
    int j;
    for (j = p; j > left && lt(tmp,a[j-1]); j-- )
      a[j] = a[j-1];
    a[j] = tmp;
  }
}

static union $Bytes8 median3(union $Bytes8 *a, int left, int right, bool (*lt)(union $Bytes8,union $Bytes8)) {
  int center = (left+right)/2;
  if (lt(a[center],a[left]))
    $SWAP(a[left],a[center])
  if (lt(a[right],a[left]))
    $SWAP(a[left],a[right])
  if (lt(a[right],a[center]))
    $SWAP(a[center],a[right])
  $SWAP(a[center],a[right-1]) // Place pivot at position right-1
  return a[right-1];
}

 

void quickselect(union $Bytes8 *a, int left, int right, int k, bool (*lt)(union $Bytes8,union $Bytes8)) {
  if(left + 10 <= right) {
    union $Bytes8 pivot = median3(a,left,right,lt);
    // Begin partitioning
    int i = left, j = right - 1;
    for (;;) {
      while (lt(a[++i],pivot)) {}
      while (lt(pivot,a[--j])) {}
      if(i<j)
        $SWAP(a[i],a[j])
      else
        break;
    }
    $SWAP(a[i], a[right-1])  // Restore pivot
    if (i==k)
      return;
    else if (i>k)
      quickselect(a,left,i-1,k,lt);
    else
      quickselect(a,i+1,right,k,lt);
  } else
    insertionsort(a,left,right,lt);
}

void quicksort(union $Bytes8 *a, int left, int right, bool (*lt)(union $Bytes8,union $Bytes8)) {
  if(left + 10 <= right) {
    union $Bytes8 pivot = median3(a,left,right,lt);
    // Begin partitioning
    int i = left, j = right - 1;
    for (;;) {
      while (lt(a[++i],pivot)) {}
      while (lt(pivot,a[--j])) {}
      if(i<j)
        $SWAP(a[i],a[j])
      else
        break;
    }
    $SWAP(a[i], a[right-1]) 
      quicksort(a,left,i-1,lt);  
    quicksort(a,i+1,right,lt); 
  } else
    insertionsort(a,left,right,lt);
}

/*
int main(int argc, char *argv[]) {
  long iters;
  sscanf(argv[1],"%ld",&iters);
  union $Bytes8 a[100];
  
  for (int r = 0; r<iters; r++) {
    for (int i=0; i<100; i++)
      a[i].l = rand()%90+10;
    quickselect(a,0,99,60,l$lt);
    for (int i=0; i<100; i++)
      printf("%ld ",a[i].l);
    printf("\n\n");
  }
}

$ndarray $ndarray_partition($ndarray a, $int k, $int axis) {
*/
