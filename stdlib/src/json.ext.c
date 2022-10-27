
#include "../rts/io.h"
#include "../rts/log.h"
#include "../deps/yyjson.h"

void json$$__ext_init__() {
    // NOP
}

void json$$encode_list(yyjson_mut_doc *doc, yyjson_mut_val *node, $list data);
void json$$encode_dict(yyjson_mut_doc *doc, yyjson_mut_val *node, $dict data) {
    $Iterator$dict$items iter = $NEW($Iterator$dict$items, data);
    $tuple item;

    for (int i=0; i < $dict_len(data); i++) {
        item = ($tuple)iter->$class->__next__(iter);
        char *key = from$str(($str)item->components[0]);
        char *value = from$str(($str)item->components[1]);
        $value v = item->components[1];
        //log_info("key: %s  class_id: %d  type: %s", key, v->$class->$class_id, v->$class->$GCINFO);
        switch (v->$class->$class_id) {
            case 2:; // $int
                yyjson_mut_obj_add_int(doc, node, key, from$int(($int)v));
                break;
            case 5:; // $bool
                yyjson_mut_obj_add_bool(doc, node, key, from$bool(($bool)v));
                break;
            case 6:; // $str
                yyjson_mut_obj_add_str(doc, node, key, from$str(($str)v));
                break;
            case 7:; // $list
                yyjson_mut_val *l = yyjson_mut_arr(doc);
                yyjson_mut_obj_add_val(doc, node, key, l);
                json$$encode_list(doc, l, ($list)v);
                break;
            case 8:; // $dict
                yyjson_mut_val *d = yyjson_mut_obj(doc);
                yyjson_mut_obj_add_val(doc, node, key, d);
                json$$encode_dict(doc, d, ($dict)v);
                break;
        }


    }
}

void json$$encode_list(yyjson_mut_doc *doc, yyjson_mut_val *node, $list data) {
    for (int i = 0; i < $list_len(data); i++) {
        $value v = $list_getitem(data, i);
        switch (v->$class->$class_id) {
            case 2:; // $int
                yyjson_mut_arr_add_int(doc, node, from$int(($int)v));
                break;
            case 5:; // $bool
                yyjson_mut_arr_add_bool(doc, node, from$bool(($bool)v));
                break;
            case 6:; // $str
                yyjson_mut_arr_add_str(doc, node, from$str(($str)v));
                break;
            case 7:; // $list
                yyjson_mut_val *l = yyjson_mut_arr_add_arr(doc, node);
                if (l) {
                    json$$encode_list(doc, l, ($list)v);
                } else {
                    // TODO: raise exception
                }
                break;
            case 8:; // $dict
                yyjson_mut_val *d = yyjson_mut_arr_add_obj(doc, node);
                if (d) {
                    json$$encode_dict(doc, d, ($dict)v);
                } else {
                    // TODO: raise exception
                }
                break;
        }
    }
}

$R json$$Json$encode$local (json$$Json __self__, $Cont c$cont, $dict data) {
    // Create JSON document
    yyjson_mut_doc *doc = yyjson_mut_doc_new(NULL);
    yyjson_mut_val *root = yyjson_mut_obj(doc);
    yyjson_mut_doc_set_root(doc, root);

    // TODO: do the thing
    json$$encode_dict(doc, root, data);

    const char *json = yyjson_mut_write(doc, 0, NULL);
    //yyjson_doc_free(doc);
    return $R_CONT(c$cont, to$str(json));
}

$list json$$decode_arr(yyjson_val *);

$dict json$$decode_obj(yyjson_val *obj) {

    $Hashable wit = ($Hashable)$Hashable$str$witness;
    $dict res = $NEW($dict, wit, NULL, NULL);
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
                $dict_setitem(res, wit, to$str(yyjson_get_str(key)), $None);
                break;
            case YYJSON_TYPE_BOOL:;
                $dict_setitem(res, wit, to$str(yyjson_get_str(key)), to$bool(yyjson_get_bool(val)));
                break;
            case YYJSON_TYPE_NUM:;
                $dict_setitem(res, wit, to$str(yyjson_get_str(key)), to$int(yyjson_get_int(val)));
                break;
            case YYJSON_TYPE_STR:;
                $dict_setitem(res, wit, to$str(yyjson_get_str(key)), to$str(yyjson_get_str(val)));
                break;
            case YYJSON_TYPE_ARR:;
                $list l = json$$decode_arr(val);
                $dict_setitem(res, wit, to$str(yyjson_get_str(key)), l);
                break;
            case YYJSON_TYPE_OBJ:;
                $dict d = json$$decode_obj(val);
                $dict_setitem(res, wit, to$str(yyjson_get_str(key)), d);
                break;
        }
    }
    return res;
}

$list json$$decode_arr(yyjson_val *arr) {
    $list res = $list$new(NULL, NULL);
    yyjson_val *val;
    yyjson_arr_iter iter;
    yyjson_arr_iter_init(arr, &iter);
    while ((val = yyjson_arr_iter_next(&iter))) {
        switch (yyjson_get_type(val)) {
            case YYJSON_TYPE_NONE:
                break;
            case YYJSON_TYPE_NULL:;
                // TODO: this is broken?
                $list_append(res, $None);
                break;
            case YYJSON_TYPE_BOOL:;
                $list_append(res, to$bool(yyjson_get_bool(val)));
                break;
            case YYJSON_TYPE_NUM:;
                $list_append(res, to$int(yyjson_get_int(val)));
                break;
            case YYJSON_TYPE_STR:;
                $list_append(res, to$str(yyjson_get_str(val)));
                break;
            case YYJSON_TYPE_ARR:;
                $list l = json$$decode_arr(val);
                $list_append(res, l);
                break;
            case YYJSON_TYPE_OBJ:;
                $dict d = json$$decode_obj(val);
                $list_append(res, d);
                break;
        }
    }
    return res;
}

$R json$$Json$decode$local (json$$Json __self__, $Cont c$cont, $str data) {
    // Read JSON and get root
    yyjson_read_err err;
    yyjson_doc *doc = yyjson_read_opts(from$str(data), strlen(from$str(data)), 0, NULL, &err);
    yyjson_val *root = yyjson_doc_get_root(doc);

    $dict res = $NEW($dict,($Hashable)$Hashable$str$witness,NULL,NULL);
    // Iterate over the root object
    if (doc) {
        yyjson_val *obj = yyjson_doc_get_root(doc);
        res = json$$decode_obj(obj);
    } else {
        char errmsg[1024];
        snprintf(errmsg, sizeof(errmsg), "JSON parsing error: %s (%u) at position %ld", err.msg, err.code, err.pos);
        $RAISE(($BaseException)$NEW($ValueError, to$str(errmsg)));
    }

    yyjson_doc_free(doc);
    return $R_CONT(c$cont, res);
}
