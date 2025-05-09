struct B_hasher;
typedef struct B_hasher *B_hasher;

/*
struct B_hasherG_class {
    char *$GCINFO;
    int $class_id;
    $SuperG_class $superclass;
    B_NoneType (*__init__) (B_hasher, u64);
    void (*__serialize__) (B_hasher, $Serial$state);
    B_hasher (*__deserialize__) (B_hasher, $Serial$state);
    B_bool (*__bool__) (B_hasher);
    B_str (*__str__) (B_hasher);
    B_str (*__repr__) (B_hasher);
    B_NoneType (*update) (B_hasher, B_bytes);
    B_u64 (*finalize) (B_hasher);
};
*/

struct B_hasher {
    struct B_hasherG_class *$class;
    void  *_hasher;
};

extern struct B_hasherG_class B_hasherG_methods;
B_hasher B_hasherG_new(B_u64);

void *zig_hash_wyhash_init(uint64_t seed);
void zig_hash_wyhash_update(void *hasher, B_bytes data);
uint64_t zig_hash_wyhash_final(void *hasher);

uint64_t zig_hash_wyhash_hash(uint64_t seed, B_bytes data);

