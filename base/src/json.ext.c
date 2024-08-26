
#include "../rts/io.h"
#include "../rts/log.h"
#include "yyjson.h"

static void *my_malloc(void *ctx, size_t size) {
    return acton_malloc(size);
}

static void *my_realloc(void *ctx, void *ptr, size_t size) {
    return acton_realloc(ptr, size);
}

static void my_free(void *ctx, void *ptr) {
    acton_free(ptr);
}


yyjson_alc acton_alc;

void jsonQ___ext_init__() {
    acton_alc.malloc = my_malloc;
    acton_alc.realloc = my_realloc;
    acton_alc.free = my_free;
}

void jsonQ_encode_list(yyjson_mut_doc *doc, yyjson_mut_val *node, B_list data);
void jsonQ_encode_dict(yyjson_mut_doc *doc, yyjson_mut_val *node, B_dict data) {
    B_IteratorD_dict_items iter = $NEW(B_IteratorD_dict_items, data);
    B_tuple item;

    for (int i=0; i < data->numelements; i++) {
        item = (B_tuple)iter->$class->__next__(iter);
        char *key = (char *)fromB_str((B_str)item->components[0]);
        B_value v = item->components[1];
        if (v) {
            switch (v->$class->$class_id) {
                case INT_ID:;
                    yyjson_mut_obj_add_int(doc, node, key, from$int((B_int)v));
                    break;
                case FLOAT_ID:;
                    yyjson_mut_obj_add_real(doc, node, key, fromB_float((B_float)v));
                    break;
                case BOOL_ID:;
                    yyjson_mut_obj_add_bool(doc, node, key, fromB_bool((B_bool)v));
                    break;
                case STR_ID:;
                    yyjson_mut_obj_add_str(doc, node, key,  (char *)fromB_str((B_str)v));
                    break;
                case LIST_ID:;
                    yyjson_mut_val *l = yyjson_mut_arr(doc);
                    yyjson_mut_obj_add_val(doc, node, key, l);
                    jsonQ_encode_list(doc, l, (B_list)v);
                    break;
                case DICT_ID:;
                    yyjson_mut_val *d = yyjson_mut_obj(doc);
                    yyjson_mut_obj_add_val(doc, node, key, d);
                    jsonQ_encode_dict(doc, d, (B_dict)v);
                    break;
                default:;
                    // TODO: hmm, at least handle all builtin types? and that's it,
                    // maybe? like we really shouldn't accept user-defined types
                    // here, just throw an exception? or when we have unions, just
                    // accept union of the types we support
                    $RAISE(((B_BaseException)B_ValueErrorG_new($FORMAT("jsonQ_encode_dict: for key %s unknown type: %s", key, v->$class->$GCINFO))));
            }
        } else {
            yyjson_mut_obj_add_null(doc, node, key);
        }
    }
}

void jsonQ_encode_list(yyjson_mut_doc *doc, yyjson_mut_val *node, B_list data) {
    for (int i = 0; i < data->length; i++) {
        B_value v = data->data[i];
        if (v) {
            switch (v->$class->$class_id) {
                case INT_ID:;
                    yyjson_mut_arr_add_int(doc, node, from$int((B_int)v));
                    break;
                case FLOAT_ID:;
                    yyjson_mut_arr_add_real(doc, node, fromB_float((B_float)v));
                    break;
                case BOOL_ID:;
                    yyjson_mut_arr_add_bool(doc, node, fromB_bool((B_bool)v));
                    break;
                case STR_ID:;
                    yyjson_mut_arr_add_str(doc, node,  (char *)fromB_str((B_str)v));
                    break;
                case LIST_ID:;
                    yyjson_mut_val *l = yyjson_mut_arr_add_arr(doc, node);
                    if (l) {
                        jsonQ_encode_list(doc, l, (B_list)v);
                    } else {
                        // TODO: raise exception
                    }
                    break;
                case DICT_ID:;
                    yyjson_mut_val *d = yyjson_mut_arr_add_obj(doc, node);
                    if (d) {
                        jsonQ_encode_dict(doc, d, (B_dict)v);
                    } else {
                        // TODO: raise exception
                    }
                    break;
                default:;
                    // TODO: hmm, at least handle all builtin types? and that's it,
                    // maybe? like we really shouldn't accept user-defined types
                    // here, just throw an exception? or when we have unions, just
                    // accept union of the types we support
                    $RAISE(((B_BaseException)B_ValueErrorG_new($FORMAT("jsonQ_encode_list: unknown type: %s", v->$class->$GCINFO))));
            }
        } else {
            yyjson_mut_arr_add_null(doc, node);
        }
    }
}

B_list jsonQ_decode_arr(yyjson_val *);

B_dict jsonQ_decode_obj(yyjson_val *obj) {

    B_Hashable wit = (B_Hashable)B_HashableD_strG_witness;
    B_dict res = $NEW(B_dict, wit, NULL, NULL);
    yyjson_obj_iter iter;
    yyjson_obj_iter_init(obj, &iter);
    yyjson_val *key, *val;
    while ((key = yyjson_obj_iter_next(&iter))) {
        val = yyjson_obj_iter_get_val(key);

        switch (yyjson_get_type(val)) {
            case YYJSON_TYPE_NONE:;
                break;
            case YYJSON_TYPE_NULL:;
                B_dictD_setitem(res, wit, to$str(yyjson_get_str(key)), B_None);
                break;
            case YYJSON_TYPE_BOOL:;
                B_dictD_setitem(res, wit, to$str(yyjson_get_str(key)), toB_bool(yyjson_get_bool(val)));
                break;
            case YYJSON_TYPE_NUM:;
                switch (yyjson_get_subtype(val)) {
                    case YYJSON_SUBTYPE_UINT:;
                        B_dictD_setitem(res, wit, to$str(yyjson_get_str(key)), to$int(yyjson_get_int(val)));
                        break;
                    case YYJSON_SUBTYPE_SINT:;
                        B_dictD_setitem(res, wit, to$str(yyjson_get_str(key)), to$int(yyjson_get_int(val)));
                        break;
                    case YYJSON_SUBTYPE_REAL:;
                        B_dictD_setitem(res, wit, to$str(yyjson_get_str(key)), to$float(yyjson_get_real(val)));
                        break;
                }
                break;
            case YYJSON_TYPE_STR:;
                B_dictD_setitem(res, wit, to$str(yyjson_get_str(key)), to$str(yyjson_get_str(val)));
                break;
            case YYJSON_TYPE_ARR:;
                B_list l = jsonQ_decode_arr(val);
                B_dictD_setitem(res, wit, to$str(yyjson_get_str(key)), l);
                break;
            case YYJSON_TYPE_OBJ:;
                B_dict d = jsonQ_decode_obj(val);
                B_dictD_setitem(res, wit, to$str(yyjson_get_str(key)), d);
                break;
            default:;
                // unreachable
                $RAISE(((B_BaseException)B_ValueErrorG_new($FORMAT("jsonQ_encode_list: unknown type: %d", yyjson_get_type(val)))));
        }
    }
    return res;
}

B_list jsonQ_decode_arr(yyjson_val *arr) {
    B_SequenceD_list wit = B_SequenceD_listG_witness;
    B_list res = B_listG_new(NULL, NULL);
    yyjson_val *val;
    yyjson_arr_iter iter;
    yyjson_arr_iter_init(arr, &iter);
    while ((val = yyjson_arr_iter_next(&iter))) {
        switch (yyjson_get_type(val)) {
            case YYJSON_TYPE_NONE:
                break;
            case YYJSON_TYPE_NULL:;
                wit->$class->append(wit, res, B_None);
                break;
            case YYJSON_TYPE_BOOL:;
                wit->$class->append(wit, res, toB_bool(yyjson_get_bool(val)));
                break;
            case YYJSON_TYPE_NUM:;
                switch (yyjson_get_subtype(val)) {
                    case YYJSON_SUBTYPE_UINT:;
                        wit->$class->append(wit, res, to$int(yyjson_get_int(val)));
                        break;
                    case YYJSON_SUBTYPE_SINT:;
                        wit->$class->append(wit, res, to$int(yyjson_get_int(val)));
                        break;
                    case YYJSON_SUBTYPE_REAL:;
                        wit->$class->append(wit, res, to$float(yyjson_get_real(val)));
                        break;
                }
                break;
            case YYJSON_TYPE_STR:;
                wit->$class->append(wit, res, to$str(yyjson_get_str(val)));
                break;
            case YYJSON_TYPE_ARR:;
                B_list l = jsonQ_decode_arr(val);
                wit->$class->append(wit, res, l);
                break;
            case YYJSON_TYPE_OBJ:;
                B_dict d = jsonQ_decode_obj(val);
                wit->$class->append(wit, res, d);
                break;
            default:;
                // TODO: just handle all types?
                $RAISE(((B_BaseException)B_ValueErrorG_new($FORMAT("jsonQ_decode_arr: unknown type: %d", yyjson_get_type(val)))));
        }
    }
    return res;
}

B_dict jsonQ_decode (B_str data) {
    // Read JSON and get root
    yyjson_read_err err;
    yyjson_doc *doc = yyjson_read_opts(fromB_str(data), strlen(fromB_str(data)), 0, &acton_alc, &err);
    yyjson_val *root = yyjson_doc_get_root(doc);

    B_dict res = $NEW(B_dict,(B_Hashable)B_HashableD_strG_witness,NULL,NULL);
    // Iterate over the root object
    if (doc) {
        yyjson_val *obj = yyjson_doc_get_root(doc);
        res = jsonQ_decode_obj(obj);
    } else {
        char errmsg[1024];
        snprintf(errmsg, sizeof(errmsg), "JSON parsing error: %s (%u) at position %ld", err.msg, err.code, err.pos);
        $RAISE((B_BaseException)$NEW(B_ValueError, to$str(errmsg)));
    }

    yyjson_doc_free(doc);
    return res;
}

B_str jsonQ_encode (B_dict data, B_bool pretty) {
    if (pretty == NULL)
        pretty = B_False;

    // Create JSON document
    yyjson_mut_doc *doc = yyjson_mut_doc_new(&acton_alc);
    yyjson_mut_val *root = yyjson_mut_obj(doc);
    yyjson_mut_doc_set_root(doc, root);

    jsonQ_encode_dict(doc, root, data);

    yyjson_write_err err;
    int flags = 0;
    if (pretty == B_True)
        flags += YYJSON_WRITE_PRETTY;

    char *json = yyjson_mut_write_opts(doc, flags, &acton_alc, NULL, &err);
    //yyjson_doc_free(doc);
    return to$str(json);
}
