
long B_i64D_hash(B_i64 n);
long B_i32D_hash(B_i32 n);
long B_i16D_hash(B_i16 n);
long B_floatD_hash(B_float v);
long B_complexD_hash(B_complex c);

long B_string_hash(B_str s);
long B_bytesD_hash(B_bytes s);

long $pointer_hash($WORD w);
long B_tupleD_hash(B_HashableD_tuple wit,B_tuple tup);

long long_hash (long u);
