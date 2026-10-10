
#include "../rts/io.h"
#include "../rts/log.h"
#include "yyjson.h"

static struct B_ValueError jsonQ_float_out_of_range_error =
    STATIC_EXCEPTION(B_ValueError, "JSON floating-point number is out of range");
static struct B_ValueError jsonQ_root_not_object_error =
    STATIC_EXCEPTION(B_ValueError, "JSON root is not an object");
static struct B_ValueError jsonQ_root_not_array_error =
    STATIC_EXCEPTION(B_ValueError, "JSON root is not an array");
static struct B_MemoryError jsonQ_allocation_failed_error =
    STATIC_EXCEPTION(B_MemoryError, "memory allocation failed");

// yyjson allocates its documents, value pools, string copies, parse buffers
// and the encoded text with malloc, its default allocator, which is selected
// by passing NULL as the allocator. None of yyjson's memory is on the GC
// heap. The GC does not scan malloc memory, so a document must never hold the
// only reference to a GC object. Each document is freed before its function
// returns or raises. The conversions therefore return their errors through an
// out-parameter, and the caller frees the document and then raises the error.

void jsonQ___ext_init__() {
}

static B_BaseException jsonQ_encode_dict(yyjson_mut_doc *doc, yyjson_mut_val *node, B_dict data, bool bigint_as_string);
static B_BaseException jsonQ_encode_list_into(yyjson_mut_doc *doc, yyjson_mut_val *node, B_list data, bool bigint_as_string);

// get_str returns the digits in a GC allocation that nothing else refers to,
// so they are copied into the document.
static yyjson_mut_val *jsonQ_encode_bigint(yyjson_mut_doc *doc, B_bigint value, bool as_string) {
    char *number = get_str(&value->val);
    if (as_string)
        return yyjson_mut_strcpy(doc, number);
    return yyjson_mut_rawcpy(doc, number);
}

// Builds the yyjson value for v. key is the dict key v is stored under, or
// NULL when v is a list element or the root, and is named in the error for an
// unsupported type. Returns NULL with *error set when v or a value in it can't
// be encoded or yyjson can't allocate memory.
//
// A str value refers to the bytes of the str object without copying them. The
// object is reachable from the data being encoded, which jsonQ_encode_root
// keeps reachable until the document has been written.
static yyjson_mut_val *jsonQ_encode_value(yyjson_mut_doc *doc, B_value v, B_str key, bool bigint_as_string, B_BaseException *error) {
    yyjson_mut_val *val;
    if (!v) {
        val = yyjson_mut_null(doc);
    } else {
        switch (v->$class->$class_id) {
            case INT_ID:
                val = yyjson_mut_sint(doc, fromB_int((B_int)v));
                break;
            case FLOAT_ID:
                val = yyjson_mut_real(doc, fromB_float((B_float)v));
                break;
            case BOOL_ID:
                val = yyjson_mut_bool(doc, fromB_bool((B_bool)v));
                break;
            case STR_ID:
                val = yyjson_mut_strn(doc, (char *)fromB_str((B_str)v), ((B_str)v)->nbytes);
                break;
            case BIGINT_ID:
                val = jsonQ_encode_bigint(doc, (B_bigint)v, bigint_as_string);
                break;
            case LIST_ID:
                val = yyjson_mut_arr(doc);
                if (val && (*error = jsonQ_encode_list_into(doc, val, (B_list)v, bigint_as_string)))
                    return NULL;
                break;
            case DICT_ID:
                val = yyjson_mut_obj(doc);
                if (val && (*error = jsonQ_encode_dict(doc, val, (B_dict)v, bigint_as_string)))
                    return NULL;
                break;
            case I8_ID:
                val = yyjson_mut_sint(doc, ((B_i8)v)->val);
                break;
            case I16_ID:
                val = yyjson_mut_sint(doc, ((B_i16)v)->val);
                break;
            case I32_ID:
                val = yyjson_mut_sint(doc, ((B_i32)v)->val);
                break;
            case I64_ID:
                val = yyjson_mut_sint(doc, ((B_int)v)->val);
                break;
            case U1_ID:
                val = yyjson_mut_uint(doc, ((B_u1)v)->val);
                break;
            case U8_ID:
                val = yyjson_mut_uint(doc, ((B_u8)v)->val);
                break;
            case U16_ID:
                val = yyjson_mut_uint(doc, ((B_u16)v)->val);
                break;
            case U32_ID:
                val = yyjson_mut_uint(doc, ((B_u32)v)->val);
                break;
            case U64_ID:
                val = yyjson_mut_uint(doc, ((B_u64)v)->val);
                break;
            default:
                // TODO: hmm, at least handle all builtin types? and that's it,
                // maybe? like we really shouldn't accept user-defined types
                // here, just throw an exception? or when we have unions, just
                // accept union of the types we support
                if (key) {
                    B_str message = B_TimesD_strD___add__(B_TimesD_strG_witness,
                        actStrFromCString("jsonQ_encode_dict: for key "), key);
                    *error = (B_BaseException)$NEW(B_ValueError, B_TimesD_strD___add__(B_TimesD_strG_witness,
                        message, $FORMAT(" unknown type: %s", v->$class->$GCINFO)));
                } else {
                    *error = (B_BaseException)$NEW(B_ValueError,
                        $FORMAT("jsonQ_encode_list: unknown type: %s", v->$class->$GCINFO));
                }
                return NULL;
        }
    }
    if (!val)
        *error = (B_BaseException)&jsonQ_allocation_failed_error;
    return val;
}

static B_BaseException jsonQ_encode_dict(yyjson_mut_doc *doc, yyjson_mut_val *node, B_dict data, bool bigint_as_string) {
    B_IteratorD_dict_items iter = $NEW(B_IteratorD_dict_items, data);
    for (int64_t i = 0; i < data->numelements; i++) {
        $WORD next;
        iter->$class->__next__(iter, &next);
        B_tuple item = (B_tuple)next;
        B_str name = (B_str)item->components[0];
        yyjson_mut_val *key = yyjson_mut_strn(doc, (char *)name->str, name->nbytes);
        if (!key)
            return (B_BaseException)&jsonQ_allocation_failed_error;
        B_BaseException error = NULL;
        yyjson_mut_val *val = jsonQ_encode_value(doc, item->components[1], name, bigint_as_string, &error);
        if (!val)
            return error;
        yyjson_mut_obj_add(node, key, val);
    }
    return NULL;
}

static B_BaseException jsonQ_encode_list_into(yyjson_mut_doc *doc, yyjson_mut_val *node, B_list data, bool bigint_as_string) {
    for (int64_t i = 0; i < data->length; i++) {
        B_BaseException error = NULL;
        yyjson_mut_val *val = jsonQ_encode_value(doc, data->data[i], NULL, bigint_as_string, &error);
        if (!val)
            return error;
        yyjson_mut_arr_append(node, val);
    }
    return NULL;
}

// Encodes data, a dict or a list, as JSON text.
static B_str jsonQ_encode_root(B_value data, bool pretty, bool bigint_as_string) {
    yyjson_mut_doc *doc = yyjson_mut_doc_new(NULL);
    if (!doc)
        RAISE_EXC(&jsonQ_allocation_failed_error);
    B_BaseException error = NULL;
    yyjson_mut_val *root = jsonQ_encode_value(doc, data, NULL, bigint_as_string, &error);
    if (!root) {
        yyjson_mut_doc_free(doc);
        RAISE_EXC(error);
    }
    yyjson_mut_doc_set_root(doc, root);

    yyjson_write_err err;
    size_t len;
    char *json = yyjson_mut_write_opts(doc, pretty ? YYJSON_WRITE_PRETTY : YYJSON_WRITE_NOFLAG, NULL, &len, &err);
    // The document refers to the bytes of str objects in data up to here.
    GC_reachable_here(data);
    yyjson_mut_doc_free(doc);
    if (!json) {
        if (err.code == YYJSON_WRITE_ERROR_MEMORY_ALLOCATION)
            RAISE_EXC(&jsonQ_allocation_failed_error);
        RAISE(B_ValueError, $FORMAT("JSON encoding error: %s", err.msg));
    }
    // The writer rejects invalid UTF-8 in strings and writes only bigint digits
    // as raw text, so json is valid UTF-8 and the conversion does not raise.
    B_str res = actStrFromCStringLengthCopy(json, len);
    free(json);
    return res;
}

static B_dict jsonQ_decode_obj(yyjson_val *obj, B_BaseException *error);
static B_list jsonQ_decode_arr(yyjson_val *arr, B_BaseException *error);

// Returns NULL with *error set for a number too large for a float, or when
// malloc fails.
static B_value jsonQ_decode_integer(yyjson_val *val, B_BaseException *error) {
    if (yyjson_is_raw(val)) {
        size_t len = yyjson_get_len(val);
        const char *raw = yyjson_get_raw(val);
        for (size_t i = 0; i < len; i++) {
            if (raw[i] == '.' || raw[i] == 'e' || raw[i] == 'E') {
                *error = (B_BaseException)&jsonQ_float_out_of_range_error;
                return NULL;
            }
        }
        // Any other raw number is a JSON integer, digits after an optional
        // minus sign, which toB_bigint2 parses without raising.
        char *number = malloc(len + 1);
        if (!number) {
            *error = (B_BaseException)&jsonQ_allocation_failed_error;
            return NULL;
        }
        memcpy(number, raw, len);
        number[len] = '\0';
        B_bigint res = toB_bigint2(number);
        free(number);
        return (B_value)res;
    }
    if (yyjson_get_subtype(val) == YYJSON_SUBTYPE_UINT) {
        uint64_t number = yyjson_get_uint(val);
        if (number > INT64_MAX)
            return (B_value)toB_u64(number);
        return (B_value)toB_int((int64_t)number);
    }
    return (B_value)toB_int(yyjson_get_sint(val));
}

// Converts val to Acton values. The result for JSON null is None, which is
// also NULL, so callers check *error, which is set when val or a value in it
// can't be decoded.
//
// yyjson rejects invalid UTF-8 and lone surrogate escapes, so the strings are
// valid UTF-8 and their conversion does not raise.
static B_value jsonQ_decode_value(yyjson_val *val, B_BaseException *error) {
    switch (yyjson_get_type(val)) {
        case YYJSON_TYPE_RAW:
            return jsonQ_decode_integer(val, error);
        case YYJSON_TYPE_NULL:
            return B_None;
        case YYJSON_TYPE_BOOL:
            return (B_value)toB_bool(yyjson_get_bool(val));
        case YYJSON_TYPE_NUM:
            if (yyjson_get_subtype(val) == YYJSON_SUBTYPE_REAL)
                return (B_value)to$float(yyjson_get_real(val));
            return jsonQ_decode_integer(val, error);
        case YYJSON_TYPE_STR:
            return (B_value)actStrFromCStringLengthCopy(yyjson_get_str(val), yyjson_get_len(val));
        case YYJSON_TYPE_ARR:
            return (B_value)jsonQ_decode_arr(val, error);
        case YYJSON_TYPE_OBJ:
            return (B_value)jsonQ_decode_obj(val, error);
        default:
            // unreachable
            *error = (B_BaseException)$NEW(B_ValueError, $FORMAT("jsonQ_decode: unknown type: %d", yyjson_get_type(val)));
            return NULL;
    }
}

static B_dict jsonQ_decode_obj(yyjson_val *obj, B_BaseException *error) {
    B_Hashable wit = (B_Hashable)B_HashableD_strG_witness;
    B_dict res = $NEW(B_dict, wit, NULL, NULL);
    yyjson_obj_iter iter;
    yyjson_obj_iter_init(obj, &iter);
    yyjson_val *key;
    while ((key = yyjson_obj_iter_next(&iter))) {
        B_str name = actStrFromCStringLengthCopy(yyjson_get_str(key), yyjson_get_len(key));
        B_value v = jsonQ_decode_value(yyjson_obj_iter_get_val(key), error);
        if (*error)
            return NULL;
        B_dictD_setitem(res, wit, name, v);
    }
    return res;
}

static B_list jsonQ_decode_arr(yyjson_val *arr, B_BaseException *error) {
    B_SequenceD_list wit = B_SequenceD_listG_witness;
    B_list res = B_listG_new(NULL, NULL);
    yyjson_val *val;
    yyjson_arr_iter iter;
    yyjson_arr_iter_init(arr, &iter);
    while ((val = yyjson_arr_iter_next(&iter))) {
        B_value v = jsonQ_decode_value(val, error);
        if (*error)
            return NULL;
        wit->$class->append(wit, res, v);
    }
    return res;
}

// Parses data into a document, which the caller frees.
static yyjson_doc *jsonQ_read(B_str data) {
    yyjson_read_err err;
    yyjson_doc *doc = yyjson_read_opts((char *)fromB_str(data), data->nbytes, YYJSON_READ_BIGNUM_AS_RAW, NULL, &err);
    if (!doc) {
        char errmsg[1024];
        snprintf(errmsg, sizeof(errmsg), "JSON parsing error: %s (%u) at position %ld", err.msg, err.code, err.pos);
        RAISE(B_ValueError, actStrFromCStringCopy(errmsg));
    }
    return doc;
}

B_dict jsonQ__decode (B_str data) {
    yyjson_doc *doc = jsonQ_read(data);
    yyjson_val *root = yyjson_doc_get_root(doc);
    if (yyjson_get_type(root) != YYJSON_TYPE_OBJ) {
        yyjson_doc_free(doc);
        RAISE_EXC(&jsonQ_root_not_object_error);
    }
    B_BaseException error = NULL;
    B_dict res = jsonQ_decode_obj(root, &error);
    yyjson_doc_free(doc);
    if (error)
        RAISE_EXC(error);
    return res;
}

B_list jsonQ__decode_list (B_str data) {
    yyjson_doc *doc = jsonQ_read(data);
    yyjson_val *root = yyjson_doc_get_root(doc);
    if (yyjson_get_type(root) != YYJSON_TYPE_ARR) {
        yyjson_doc_free(doc);
        RAISE_EXC(&jsonQ_root_not_array_error);
    }
    B_BaseException error = NULL;
    B_list res = jsonQ_decode_arr(root, &error);
    yyjson_doc_free(doc);
    if (error)
        RAISE_EXC(error);
    return res;
}

B_str jsonQ__encode (B_dict data, bool pretty, bool bigint_as_string) {
    return jsonQ_encode_root((B_value)data, pretty, bigint_as_string);
}

B_str jsonQ__encode_list (B_list data, bool pretty, bool bigint_as_string) {
    return jsonQ_encode_root((B_value)data, pretty, bigint_as_string);
}
