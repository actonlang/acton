#include "../../builtin/builtin.h"
#include "../ndarray.h"

int main() {
  $ndarray a = $NEW($ndarray,to$float(3.1416));
  $print($NEW($tuple,2,to$str("a="),a));
}
