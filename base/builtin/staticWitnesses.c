struct B_HashableD_bytes B_HashableD_bytesG_instance;
struct B_TimesD_bytes B_TimesD_bytesG_instance;
struct B_ContainerD_bytes B_ContainerD_bytesG_instance;
struct B_SliceableD_bytes B_SliceableD_bytesG_instance;
struct B_OrdD_bytes B_OrdD_bytesG_instance;
struct B_ContainerD_bytearray B_ContainerD_bytearrayG_instance;
struct B_TimesD_SequenceD_bytearray B_TimesD_SequenceD_bytearrayG_instance;
struct B_CollectionD_SequenceD_bytearray B_CollectionD_SequenceD_bytearrayG_instance;
struct B_SequenceD_bytearray B_SequenceD_bytearrayG_instance;
struct B_OrdD_bytearray B_OrdD_bytearrayG_instance;
struct B_HashableD_str B_HashableD_strG_instance;
struct B_TimesD_str B_TimesD_strG_instance;
struct B_SliceableD_str B_SliceableD_strG_instance;
struct B_ContainerD_str B_ContainerD_strG_instance;
struct B_OrdD_str B_OrdD_strG_instance;
struct B_IterableD_range B_IterableD_rangeG_instance;
struct B_IterableD_Iterator B_IterableD_IteratorG_instance;
struct B_TimesD_SequenceD_list B_TimesD_SequenceD_listG_instance;
struct B_CollectionD_SequenceD_list B_CollectionD_SequenceD_listG_instance;
struct B_SequenceD_list B_SequenceD_listG_instance;
struct B_HashableD_complex B_HashableD_complexG_instance;
struct B_EqD_complex B_EqD_complexG_instance;
struct B_DivD_complex B_DivD_complexG_instance;
struct B_MinusD_NumberD_complex B_MinusD_NumberD_complexG_instance;
struct B_NumberD_complex B_NumberD_complexG_instance;
struct B_HashableD_float B_HashableD_floatG_instance;
struct B_OrdD_float B_OrdD_floatG_instance;
struct B_DivD_float B_DivD_floatG_instance;
struct B_MinusD_RealFloatD_float B_MinusD_RealFloatD_floatG_instance;
struct B_RealFloatD_float B_RealFloatD_floatG_instance;
struct B_HashableD_u16 B_HashableD_u16G_instance;
struct B_OrdD_u16 B_OrdD_u16G_instance;
struct B_DivD_u16 B_DivD_u16G_instance;
struct B_LogicalD_IntegralD_u16 B_LogicalD_IntegralD_u16G_instance;
struct B_MinusD_IntegralD_u16 B_MinusD_IntegralD_u16G_instance;
struct B_IntegralD_u16 B_IntegralD_u16G_instance;
struct B_HashableD_u32 B_HashableD_u32G_instance;
struct B_OrdD_u32 B_OrdD_u32G_instance;
struct B_DivD_u32 B_DivD_u32G_instance;
struct B_LogicalD_IntegralD_u32 B_LogicalD_IntegralD_u32G_instance;
struct B_MinusD_IntegralD_u32 B_MinusD_IntegralD_u32G_instance;
struct B_IntegralD_u32 B_IntegralD_u32G_instance;
struct B_HashableD_u64 B_HashableD_u64G_instance;
struct B_OrdD_u64 B_OrdD_u64G_instance;
struct B_DivD_u64 B_DivD_u64G_instance;
struct B_LogicalD_IntegralD_u64 B_LogicalD_IntegralD_u64G_instance;
struct B_MinusD_IntegralD_u64 B_MinusD_IntegralD_u64G_instance;
struct B_IntegralD_u64 B_IntegralD_u64G_instance;
struct B_HashableD_i16 B_HashableD_i16G_instance;
struct B_OrdD_i16 B_OrdD_i16G_instance;
struct B_DivD_i16 B_DivD_i16G_instance;
struct B_LogicalD_IntegralD_i16 B_LogicalD_IntegralD_i16G_instance;
struct B_MinusD_IntegralD_i16 B_MinusD_IntegralD_i16G_instance;
struct B_IntegralD_i16 B_IntegralD_i16G_instance;
struct B_HashableD_i32 B_HashableD_i32G_instance;
struct B_OrdD_i32 B_OrdD_i32G_instance;
struct B_DivD_i32 B_DivD_i32G_instance;
struct B_LogicalD_IntegralD_i32 B_LogicalD_IntegralD_i32G_instance;
struct B_MinusD_IntegralD_i32 B_MinusD_IntegralD_i32G_instance;
struct B_IntegralD_i32 B_IntegralD_i32G_instance;
struct B_HashableD_i64 B_HashableD_i64G_instance;
struct B_OrdD_i64 B_OrdD_i64G_instance;
struct B_DivD_i64 B_DivD_i64G_instance;
struct B_LogicalD_IntegralD_i64 B_LogicalD_IntegralD_i64G_instance;
struct B_MinusD_IntegralD_i64 B_MinusD_IntegralD_i64G_instance;
struct B_IntegralD_i64 B_IntegralD_i64G_instance;
struct B_HashableD_int B_HashableD_intG_instance;
struct B_OrdD_int B_OrdD_intG_instance;
struct B_DivD_int B_DivD_intG_instance;
struct B_LogicalD_IntegralD_int B_LogicalD_IntegralD_intG_instance;
struct B_MinusD_IntegralD_int B_MinusD_IntegralD_intG_instance;
struct B_IntegralD_int B_IntegralD_intG_instance;
struct B_HashableD_bool B_HashableD_boolG_instance;

struct B_HashableD_bytes B_HashableD_bytesG_instance = {&B_HashableD_bytesG_methods};
struct B_TimesD_bytes B_TimesD_bytesG_instance = {&B_TimesD_bytesG_methods};
struct B_ContainerD_bytes B_ContainerD_bytesG_instance = {&B_ContainerD_bytesG_methods, (B_Eq)&B_OrdD_intG_instance};
struct B_SliceableD_bytes B_SliceableD_bytesG_instance = {&B_SliceableD_bytesG_methods, (B_Eq)&B_OrdD_intG_instance};
struct B_OrdD_bytes B_OrdD_bytesG_instance = {&B_OrdD_bytesG_methods};

struct B_ContainerD_bytearray B_ContainerD_bytearrayG_instance = {&B_ContainerD_bytearrayG_methods, (B_Eq)&B_OrdD_intG_instance};
struct B_TimesD_SequenceD_bytearray B_TimesD_SequenceD_bytearrayG_instance = {&B_TimesD_SequenceD_bytearrayG_methods, (B_Sequence)&B_SequenceD_bytearrayG_instance};
struct B_CollectionD_SequenceD_bytearray B_CollectionD_SequenceD_bytearrayG_instance = {&B_CollectionD_SequenceD_bytearrayG_methods, (B_Sequence)&B_SequenceD_bytearrayG_instance};
struct B_SequenceD_bytearray B_SequenceD_bytearrayG_instance = {&B_SequenceD_bytearrayG_methods, (B_Eq)&B_OrdD_intG_instance, (B_Collection)&B_CollectionD_SequenceD_bytearrayG_instance, (B_Times)&B_TimesD_SequenceD_bytearrayG_instance};
struct B_OrdD_bytearray B_OrdD_bytearrayG_instance = {&B_OrdD_bytearrayG_methods};

struct B_HashableD_str B_HashableD_strG_instance = {&B_HashableD_strG_methods};
struct B_TimesD_str B_TimesD_strG_instance = {&B_TimesD_strG_methods};
struct B_SliceableD_str B_SliceableD_strG_instance = {&B_SliceableD_strG_methods, (B_Eq)&B_OrdD_intG_instance};
struct B_ContainerD_str B_ContainerD_strG_instance = {&B_ContainerD_strG_methods, (B_Eq)&B_OrdD_intG_instance};
struct B_OrdD_str B_OrdD_strG_instance = {&B_OrdD_strG_methods};

struct B_IterableD_range B_IterableD_rangeG_instance = {&B_IterableD_rangeG_methods};

struct B_IterableD_Iterator B_IterableD_IteratorG_instance = {&B_IterableD_IteratorG_methods};

struct B_TimesD_SequenceD_list B_TimesD_SequenceD_listG_instance = {&B_TimesD_SequenceD_listG_methods, (B_Sequence)&B_SequenceD_listG_instance};

struct B_CollectionD_SequenceD_list B_CollectionD_SequenceD_listG_instance = {&B_CollectionD_SequenceD_listG_methods, (B_Sequence)&B_SequenceD_listG_instance};

struct B_SequenceD_list B_SequenceD_listG_instance = {&B_SequenceD_listG_methods, (B_Eq)&B_OrdD_intG_methods, (B_Collection)&B_CollectionD_SequenceD_listG_instance, (B_Times)&B_TimesD_SequenceD_listG_instance};

struct B_HashableD_complex B_HashableD_complexG_instance = {&B_HashableD_complexG_methods};
struct B_EqD_complex B_EqD_complexG_instance = {&B_EqD_complexG_methods};
struct B_DivD_complex B_DivD_complexG_instance = {&B_DivD_complexG_methods};
struct B_MinusD_NumberD_complex B_MinusD_NumberD_complexG_instance = {&B_MinusD_NumberD_complexG_methods, (B_Number)&B_NumberD_complexG_instance};
struct B_NumberD_complex B_NumberD_complexG_instance = {&B_NumberD_complexG_methods, (B_Minus)&B_MinusD_NumberD_complexG_instance};

struct B_HashableD_float B_HashableD_floatG_instance = {&B_HashableD_floatG_methods};
struct B_OrdD_float B_OrdD_floatG_instance = {&B_OrdD_floatG_methods};
struct B_DivD_float B_DivD_floatG_instance = {&B_DivD_floatG_methods};
struct B_MinusD_RealFloatD_float B_MinusD_RealFloatD_floatG_instance = {&B_MinusD_RealFloatD_floatG_methods, (B_Number)&B_RealFloatD_floatG_instance};
struct B_RealFloatD_float B_RealFloatD_floatG_instance = {&B_RealFloatD_floatG_methods, (B_Minus)&B_MinusD_RealFloatD_floatG_instance};

struct B_HashableD_u16 B_HashableD_u16G_instance = {&B_HashableD_u16G_methods};
struct B_OrdD_u16 B_OrdD_u16G_instance = {&B_OrdD_u16G_methods};
struct B_DivD_u16 B_DivD_u16G_instance = {&B_DivD_u16G_methods};
struct B_LogicalD_IntegralD_u16 B_LogicalD_IntegralD_u16G_instance = {&B_LogicalD_IntegralD_u16G_methods, (B_Integral)&B_IntegralD_u16G_instance};
struct B_MinusD_IntegralD_u16 B_MinusD_IntegralD_u16G_instance = {&B_MinusD_IntegralD_u16G_methods, (B_Number)&B_IntegralD_u16G_instance};
struct B_IntegralD_u16 B_IntegralD_u16G_instance = {&B_IntegralD_u16G_methods, (B_Minus)&B_MinusD_IntegralD_u16G_instance, (B_Logical)&B_LogicalD_IntegralD_u16G_instance};

struct B_HashableD_u32 B_HashableD_u32G_instance = {&B_HashableD_u32G_methods};
struct B_OrdD_u32 B_OrdD_u32G_instance = {&B_OrdD_u32G_methods};
struct B_DivD_u32 B_DivD_u32G_instance = {&B_DivD_u32G_methods};
struct B_LogicalD_IntegralD_u32 B_LogicalD_IntegralD_u32G_instance = {&B_LogicalD_IntegralD_u32G_methods, (B_Integral)&B_IntegralD_u32G_instance};
struct B_MinusD_IntegralD_u32 B_MinusD_IntegralD_u32G_instance = {&B_MinusD_IntegralD_u32G_methods, (B_Number)&B_IntegralD_u32G_instance};
struct B_IntegralD_u32 B_IntegralD_u32G_instance = {&B_IntegralD_u32G_methods, (B_Minus)&B_MinusD_IntegralD_u32G_instance, (B_Logical)&B_LogicalD_IntegralD_u32G_instance};

struct B_HashableD_u64 B_HashableD_u64G_instance = {&B_HashableD_u64G_methods};
struct B_OrdD_u64 B_OrdD_u64G_instance = {&B_OrdD_u64G_methods};
struct B_DivD_u64 B_DivD_u64G_instance = {&B_DivD_u64G_methods};
struct B_LogicalD_IntegralD_u64 B_LogicalD_IntegralD_u64G_instance = {&B_LogicalD_IntegralD_u64G_methods, (B_Integral)&B_IntegralD_u64G_instance};
struct B_MinusD_IntegralD_u64 B_MinusD_IntegralD_u64G_instance = {&B_MinusD_IntegralD_u64G_methods, (B_Number)&B_IntegralD_u64G_instance};
struct B_IntegralD_u64 B_IntegralD_u64G_instance = {&B_IntegralD_u64G_methods, (B_Minus)&B_MinusD_IntegralD_u64G_instance, (B_Logical)&B_LogicalD_IntegralD_u64G_instance};

struct B_HashableD_i16 B_HashableD_i16G_instance = {&B_HashableD_i16G_methods};
struct B_OrdD_i16 B_OrdD_i16G_instance = {&B_OrdD_i16G_methods};
struct B_DivD_i16 B_DivD_i16G_instance = {&B_DivD_i16G_methods};
struct B_LogicalD_IntegralD_i16 B_LogicalD_IntegralD_i16G_instance = {&B_LogicalD_IntegralD_i16G_methods, (B_Integral)&B_IntegralD_i16G_instance};
struct B_MinusD_IntegralD_i16 B_MinusD_IntegralD_i16G_instance = {&B_MinusD_IntegralD_i16G_methods, (B_Number)&B_IntegralD_i16G_instance};
struct B_IntegralD_i16 B_IntegralD_i16G_instance = {&B_IntegralD_i16G_methods, (B_Minus)&B_MinusD_IntegralD_i16G_instance, (B_Logical)&B_LogicalD_IntegralD_i16G_instance};

struct B_HashableD_i32 B_HashableD_i32G_instance = {&B_HashableD_i32G_methods};
struct B_OrdD_i32 B_OrdD_i32G_instance = {&B_OrdD_i32G_methods};
struct B_DivD_i32 B_DivD_i32G_instance = {&B_DivD_i32G_methods};
struct B_LogicalD_IntegralD_i32 B_LogicalD_IntegralD_i32G_instance = {&B_LogicalD_IntegralD_i32G_methods, (B_Integral)&B_IntegralD_i32G_instance};
struct B_MinusD_IntegralD_i32 B_MinusD_IntegralD_i32G_instance = {&B_MinusD_IntegralD_i32G_methods, (B_Number)&B_IntegralD_i32G_instance};
struct B_IntegralD_i32 B_IntegralD_i32G_instance = {&B_IntegralD_i32G_methods, (B_Minus)&B_MinusD_IntegralD_i32G_instance, (B_Logical)&B_LogicalD_IntegralD_i32G_instance};

struct B_HashableD_i64 B_HashableD_i64G_instance = {&B_HashableD_i64G_methods};
struct B_OrdD_i64 B_OrdD_i64G_instance = {&B_OrdD_i64G_methods};
struct B_DivD_i64 B_DivD_i64G_instance = {&B_DivD_i64G_methods};
struct B_LogicalD_IntegralD_i64 B_LogicalD_IntegralD_i64G_instance = {&B_LogicalD_IntegralD_i64G_methods, (B_Integral)&B_IntegralD_i64G_instance};
struct B_MinusD_IntegralD_i64 B_MinusD_IntegralD_i64G_instance = {&B_MinusD_IntegralD_i64G_methods, (B_Number)&B_IntegralD_i64G_instance};
struct B_IntegralD_i64 B_IntegralD_i64G_instance = {&B_IntegralD_i64G_methods, (B_Minus)&B_MinusD_IntegralD_i64G_instance, (B_Logical)&B_LogicalD_IntegralD_i64G_instance};

struct B_HashableD_int B_HashableD_intG_instance = {&B_HashableD_intG_methods};
struct B_OrdD_int B_OrdD_intG_instance = {&B_OrdD_intG_methods};
struct B_DivD_int B_DivD_intG_instance = {&B_DivD_intG_methods};
struct B_LogicalD_IntegralD_int B_LogicalD_IntegralD_intG_instance = {&B_LogicalD_IntegralD_intG_methods, (B_Integral)&B_IntegralD_intG_instance};
struct B_MinusD_IntegralD_int B_MinusD_IntegralD_intG_instance = {&B_MinusD_IntegralD_intG_methods, (B_Number)&B_IntegralD_intG_instance};
struct B_IntegralD_int B_IntegralD_intG_instance = {&B_IntegralD_intG_methods, (B_Minus)&B_MinusD_IntegralD_intG_instance, (B_Logical)&B_LogicalD_IntegralD_intG_instance};

struct B_HashableD_bool  B_HashableD_boolG_instance = {&B_HashableD_boolG_methods};

B_HashableD_bytes B_HashableD_bytesG_witness = &B_HashableD_bytesG_instance;
B_TimesD_bytes B_TimesD_bytesG_witness = &B_TimesD_bytesG_instance;
B_ContainerD_bytes B_ContainerD_bytesG_witness = &B_ContainerD_bytesG_instance;
B_SliceableD_bytes B_SliceableD_bytesG_witness = &B_SliceableD_bytesG_instance;
B_OrdD_bytes B_OrdD_bytesG_witness = &B_OrdD_bytesG_instance;
B_ContainerD_bytearray B_ContainerD_bytearrayG_witness = &B_ContainerD_bytearrayG_instance;
B_TimesD_SequenceD_bytearray B_TimesD_SequenceD_bytearrayG_witness = &B_TimesD_SequenceD_bytearrayG_instance;
B_CollectionD_SequenceD_bytearray B_CollectionD_SequenceD_bytearrayG_witness = &B_CollectionD_SequenceD_bytearrayG_instance;
B_SequenceD_bytearray B_SequenceD_bytearrayG_witness = &B_SequenceD_bytearrayG_instance;
B_OrdD_bytearray B_OrdD_bytearrayG_witness = &B_OrdD_bytearrayG_instance;
B_HashableD_str B_HashableD_strG_witness = &B_HashableD_strG_instance;
B_TimesD_str B_TimesD_strG_witness = &B_TimesD_strG_instance;
B_SliceableD_str B_SliceableD_strG_witness = &B_SliceableD_strG_instance;
B_ContainerD_str B_ContainerD_strG_witness = &B_ContainerD_strG_instance;
B_OrdD_str B_OrdD_strG_witness = &B_OrdD_strG_instance;
B_IterableD_range B_IterableD_rangeG_witness = &B_IterableD_rangeG_instance;
B_IterableD_Iterator B_IterableD_IteratorG_witness = &B_IterableD_IteratorG_instance;
B_TimesD_SequenceD_list B_TimesD_SequenceD_listG_witness = &B_TimesD_SequenceD_listG_instance;
B_CollectionD_SequenceD_list B_CollectionD_SequenceD_listG_witness = &B_CollectionD_SequenceD_listG_instance;
B_SequenceD_list B_SequenceD_listG_witness = &B_SequenceD_listG_instance;
B_HashableD_complex B_HashableD_complexG_witness = &B_HashableD_complexG_instance;
B_EqD_complex B_EqD_complexG_witness = &B_EqD_complexG_instance;
B_DivD_complex B_DivD_complexG_witness = &B_DivD_complexG_instance;
B_MinusD_NumberD_complex B_MinusD_NumberD_complexG_witness = &B_MinusD_NumberD_complexG_instance;
B_NumberD_complex B_NumberD_complexG_witness = &B_NumberD_complexG_instance;
B_HashableD_float B_HashableD_floatG_witness = &B_HashableD_floatG_instance;
B_OrdD_float B_OrdD_floatG_witness = &B_OrdD_floatG_instance;
B_DivD_float B_DivD_floatG_witness = &B_DivD_floatG_instance;
B_MinusD_RealFloatD_float B_MinusD_RealFloatD_floatG_witness = &B_MinusD_RealFloatD_floatG_instance;
B_RealFloatD_float B_RealFloatD_floatG_witness = &B_RealFloatD_floatG_instance;
B_HashableD_u16 B_HashableD_u16G_witness = &B_HashableD_u16G_instance;
B_OrdD_u16 B_OrdD_u16G_witness = &B_OrdD_u16G_instance;
B_DivD_u16 B_DivD_u16G_witness = &B_DivD_u16G_instance;
B_LogicalD_IntegralD_u16 B_LogicalD_IntegralD_u16G_witness = &B_LogicalD_IntegralD_u16G_instance;
B_MinusD_IntegralD_u16 B_MinusD_IntegralD_u16G_witness = &B_MinusD_IntegralD_u16G_instance;
B_IntegralD_u16 B_IntegralD_u16G_witness = &B_IntegralD_u16G_instance;
B_HashableD_u32 B_HashableD_u32G_witness = &B_HashableD_u32G_instance;
B_OrdD_u32 B_OrdD_u32G_witness = &B_OrdD_u32G_instance;
B_DivD_u32 B_DivD_u32G_witness = &B_DivD_u32G_instance;
B_LogicalD_IntegralD_u32 B_LogicalD_IntegralD_u32G_witness = &B_LogicalD_IntegralD_u32G_instance;
B_MinusD_IntegralD_u32 B_MinusD_IntegralD_u32G_witness = &B_MinusD_IntegralD_u32G_instance;
B_IntegralD_u32 B_IntegralD_u32G_witness = &B_IntegralD_u32G_instance;
B_HashableD_u64 B_HashableD_u64G_witness = &B_HashableD_u64G_instance;
B_OrdD_u64 B_OrdD_u64G_witness = &B_OrdD_u64G_instance;
B_DivD_u64 B_DivD_u64G_witness = &B_DivD_u64G_instance;
B_LogicalD_IntegralD_u64 B_LogicalD_IntegralD_u64G_witness = &B_LogicalD_IntegralD_u64G_instance;
B_MinusD_IntegralD_u64 B_MinusD_IntegralD_u64G_witness = &B_MinusD_IntegralD_u64G_instance;
B_IntegralD_u64 B_IntegralD_u64G_witness = &B_IntegralD_u64G_instance;
B_HashableD_i16 B_HashableD_i16G_witness = &B_HashableD_i16G_instance;
B_OrdD_i16 B_OrdD_i16G_witness = &B_OrdD_i16G_instance;
B_DivD_i16 B_DivD_i16G_witness = &B_DivD_i16G_instance;
B_LogicalD_IntegralD_i16 B_LogicalD_IntegralD_i16G_witness = &B_LogicalD_IntegralD_i16G_instance;
B_MinusD_IntegralD_i16 B_MinusD_IntegralD_i16G_witness = &B_MinusD_IntegralD_i16G_instance;
B_IntegralD_i16 B_IntegralD_i16G_witness = &B_IntegralD_i16G_instance;
B_HashableD_i32 B_HashableD_i32G_witness = &B_HashableD_i32G_instance;
B_OrdD_i32 B_OrdD_i32G_witness = &B_OrdD_i32G_instance;
B_DivD_i32 B_DivD_i32G_witness = &B_DivD_i32G_instance;
B_LogicalD_IntegralD_i32 B_LogicalD_IntegralD_i32G_witness = &B_LogicalD_IntegralD_i32G_instance;
B_MinusD_IntegralD_i32 B_MinusD_IntegralD_i32G_witness = &B_MinusD_IntegralD_i32G_instance;
B_IntegralD_i32 B_IntegralD_i32G_witness = &B_IntegralD_i32G_instance;
B_HashableD_i64 B_HashableD_i64G_witness = &B_HashableD_i64G_instance;
B_OrdD_i64 B_OrdD_i64G_witness = &B_OrdD_i64G_instance;
B_DivD_i64 B_DivD_i64G_witness = &B_DivD_i64G_instance;
B_LogicalD_IntegralD_i64 B_LogicalD_IntegralD_i64G_witness = &B_LogicalD_IntegralD_i64G_instance;
B_MinusD_IntegralD_i64 B_MinusD_IntegralD_i64G_witness = &B_MinusD_IntegralD_i64G_instance;
B_IntegralD_i64 B_IntegralD_i64G_witness = &B_IntegralD_i64G_instance;
B_HashableD_int B_HashableD_intG_witness = &B_HashableD_intG_instance;
B_OrdD_int B_OrdD_intG_witness = &B_OrdD_intG_instance;
B_DivD_int B_DivD_intG_witness = &B_DivD_intG_instance;
B_LogicalD_IntegralD_int B_LogicalD_IntegralD_intG_witness = &B_LogicalD_IntegralD_intG_instance;
B_MinusD_IntegralD_int B_MinusD_IntegralD_intG_witness = &B_MinusD_IntegralD_intG_instance;
B_IntegralD_int B_IntegralD_intG_witness = &B_IntegralD_intG_instance;
B_HashableD_bool B_HashableD_boolG_witness = &B_HashableD_boolG_instance;

struct B_OrdD_list B_OrdD_listD_bytesG_instance = {&B_OrdD_listG_methods, (B_Ord)&B_OrdD_bytesG_instance};
B_OrdD_list B_OrdD_listD_bytesG_witness = &B_OrdD_listD_bytesG_instance;

struct B_IndexedD_MappingD_dict B_IndexedD_MappingD_dictD_strG_instance;
struct B_IndexedD_MappingD_dict B_IndexedD_MappingD_dictD_intG_instance;
struct B_MappingD_dict B_MappingD_dictD_strG_instance = {&B_MappingD_dictG_methods, (B_Eq)&B_HashableD_strG_instance, (B_Eq)&B_HashableD_strG_instance,
                                                         (B_Indexed)&B_IndexedD_MappingD_dictD_strG_instance, (B_Hashable)&B_HashableD_strG_instance};
struct B_MappingD_dict B_MappingD_dictD_intG_instance = {&B_MappingD_dictG_methods, (B_Eq)&B_HashableD_intG_instance, (B_Eq)&B_HashableD_intG_instance,
                                                         (B_Indexed)&B_IndexedD_MappingD_dictD_intG_instance, (B_Hashable)&B_HashableD_intG_instance};

struct B_IndexedD_MappingD_dict B_IndexedD_MappingD_dictD_strG_instance =  {&B_IndexedD_MappingD_dictG_methods, (B_Eq)&B_HashableD_strG_instance, (B_Eq)&B_HashableD_strG_instance,
                                                         (B_Mapping)&B_MappingD_dictD_strG_instance, (B_Hashable)&B_HashableD_strG_instance};
struct B_IndexedD_MappingD_dict B_IndexedD_MappingD_dictD_intG_instance =  {&B_IndexedD_MappingD_dictG_methods, (B_Eq)&B_HashableD_intG_instance, (B_Eq)&B_HashableD_intG_instance,
                                                         (B_Mapping)&B_MappingD_dictD_intG_instance, (B_Hashable)&B_HashableD_intG_instance};

B_MappingD_dict B_MappingD_dictD_strG_witness = &B_MappingD_dictD_strG_instance;
B_MappingD_dict B_MappingD_dictD_intG_witness = &B_MappingD_dictD_intG_instance;
