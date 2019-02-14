extern "C" {
    #include "test_io.h"
}

#define CUCKOO_TABLE_NAME fd_task_map
#define CUCKOO_KEY_TYPE int
#define CUCKOO_MAPPED_TYPE task_t*
#include "deps/libcuckoo/libcuckoo-c/cuckoo_table_template.cc"
#undef CUCKOO_TABLE_NAME
#undef CUCKOO_KEY_TYPE
#undef CUCKOO_MAPPED_TYPE
