
#include "../rts/io.h"
#include "../rts/log.h"
#include "yyjson.h"

void jsonQ___ext_init__() {
    // NOP
}

void jsonQ_encode_list(yyjson_mut_doc *doc, yyjson_mut_val *node, B_list data);
void jsonQ_encode_dict(yyjson_mut_doc *doc, yyjson_mut_val *node, B_dict data) {
    B_IteratorD_dict_items iter = $NEW(B_IteratorD_dict_items, data);
    B_tuple item;

    for (int i=0; i < data->numelements; i++) {
        item = (B_tuple)iter->$class->__next__(iter);
        char *key = (char *)fromB_str((B_str)item->components[0]);
        char *value = (char *)fromB_str((B_str)item->components[1]);
        B_value v = item->components[1];
        //log_info("key: %s  class_id: %d  type: %s", key, v->$class->$class_id, v->$class->$GCINFO);
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
            default:
                // TODO: hmm, at least handle all builtin types? and that's it,
                // maybe? like we really shouldn't accept user-defined types
                // here, just throw an exception? or when we have unions, just
                // accept union of the types we support
                log_error("jsonQ_encode_dict: for key %s unknown type: %d", v->$class->$GCINFO);
        }
    }
}

void jsonQ_encode_list(yyjson_mut_doc *doc, yyjson_mut_val *node, B_list data) {
    for (int i = 0; i < data->length; i++) {
        B_value v = data->data[i];
        switch (v->$class->$class_id) {
            case INT_ID:;
                yyjson_mut_arr_add_int(doc, node, from$int((B_int)v));
                break;
            case FLOAT_ID:;
                log_debug("jsonQ_encode_list: float, value: %f", fromB_float((B_float)v));
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
            default:
                // TODO: hmm, at least handle all builtin types? and that's it,
                // maybe? like we really shouldn't accept user-defined types
                // here, just throw an exception? or when we have unions, just
                // accept union of the types we support
                log_error("jsonQ_encode_list: unknown type: %d", v->$class->$GCINFO);
        }
    }
}

$R jsonQ_JsonD_encodeG_local (jsonQ_Json self, $Cont c$cont, B_dict data) {
    // Create JSON document
    yyjson_mut_doc *doc = yyjson_mut_doc_new(NULL);
    yyjson_mut_val *root = yyjson_mut_obj(doc);
    yyjson_mut_doc_set_root(doc, root);

    // TODO: do the thing
    jsonQ_encode_dict(doc, root, data);

    char *json = yyjson_mut_write(doc, 0, NULL);
    //yyjson_doc_free(doc);
    return $R_CONT(c$cont, to$str(json));
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
                // TODO: this is broken?
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
            default:
                // TODO: just handle all types?
                log_error("jsonQ_decode_obj: unknown type: %d", yyjson_get_type(val));
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
                // TODO: this is broken?
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
            default:
                // TODO: just handle all types?
                log_error("jsonQ_decode_arr: unknown type: %d", yyjson_get_type(val));
        }
    }
    return res;
}

$R jsonQ_JsonD_decodeG_local (jsonQ_Json self, $Cont c$cont, B_str data) {
    // Read JSON and get root
    yyjson_read_err err;
    yyjson_doc *doc = yyjson_read_opts(fromB_str(data), strlen(fromB_str(data)), 0, NULL, &err);
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
    return $R_CONT(c$cont, res);
}
