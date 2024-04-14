#define LIBXML_STATIC
#include <libxml/xmlmemory.h>
#include <libxml/parser.h>

// TODO: The macro below is from builtin/str.c. We should not duplicate it...
#define NEW_UNFILLED_STR(nm, nchrs, nbtes)      \
    nm = acton_malloc(sizeof(struct B_str));           \
    (nm)->$class = &B_strG_methods;               \
    (nm)->nchars = nchrs;                       \
    (nm)->nbytes = nbtes;                       \
    (nm)->str = acton_malloc_atomic((nm)->nbytes + 1);       \
    (nm)->str[(nm)->nbytes] = 0


xmlQ_Node $NodePtr2Node(xmlNodePtr node) {
    B_SequenceD_list wit = B_SequenceD_listG_witness;
    if (node->type != XML_ELEMENT_NODE) {
        char *errmsg = NULL;
        $RAISE(((B_BaseException)B_RuntimeErrorG_new($FORMAT("Unexpected nodetype %d, content is %s", node->type, node->content))));
    }

    B_list nsdefs = B_listG_new(NULL, NULL);
    xmlNsPtr nsDef = node->nsDef;
    while (nsDef) {
        B_str prefix = NULL;
        if (nsDef->prefix) prefix = to$str((char *)nsDef->prefix);
        B_str href = to$str((char *)nsDef->href);
        wit->$class->append(wit,nsdefs, $NEWTUPLE(2, prefix, href));
        nsDef=nsDef->next;
    }

    B_str prefix = NULL;
    if (node->ns && node->ns->prefix)
        prefix = to$str((char *)node->ns->prefix);

    B_list attributes = B_listG_new(NULL, NULL);
    xmlAttrPtr attr = node->properties;
    while (attr) {
        wit->$class->append(wit,attributes, $NEWTUPLE(2, to$str((char *)attr->name), to$str((char *)xmlGetProp(node, attr->name))));
        attr = attr->next;
    }

    B_str text = to$str("");
    B_str tail = to$str("");

    B_list children = B_listG_new(NULL, NULL);
    xmlNodePtr cur = node->xmlChildrenNode;
    if (cur && cur->type == XML_TEXT_NODE) {
        text = to$str((char *)cur->content);
        cur = cur->next;
    }

    while (cur != NULL) {
        wit->$class->append(wit,children, $NodePtr2Node(cur));
        cur = cur->next;
    }

    if (node->next && node->next->type == XML_TEXT_NODE) { // the root node has no next.
        tail = to$str((char *)node->next->content);
        node->next = node->next->next;
    }
    return (xmlQ_Node)$NEW(xmlQ_Node, to$str((char *)node->name), nsdefs, prefix, attributes, children, text, tail);
}

xmlQ_Node xmlQ_decode(B_str data) {
    xmlDocPtr doc = xmlReadMemory((char *)data->str, data->nbytes, NULL, NULL, 0);
    if (!doc) {
        // xmlErrorPtr err = xmlGetLastError();
        $RAISE(((B_BaseException)B_RuntimeErrorG_new(to$str("xml parse error"))));
    }
    xmlNodePtr root = xmlDocGetRootElement(doc);
    xmlQ_Node t = $NodePtr2Node(root);
    xmlFreeDoc(doc);
    return t;
}


B_str xmlQ_node2str(B_str tag, B_str nsdefs, B_str prefix, B_str attrs, B_str cont, B_str text, B_str tail) {
    int res_bytes = 2*tag->nbytes + 2*(prefix ? prefix->nbytes+1:0) + nsdefs->nbytes + attrs->nbytes +
                    (text ? text->nbytes:0) + cont->nbytes + (tail ? tail->nbytes:0) + 5; // 5 = len ("<" + ">" + "</" + ">")
    int res_chars = 2*tag->nchars + 2*(prefix ? prefix->nchars+1:0) + nsdefs->nchars + attrs->nchars +
                    (text ? text->nchars:0) + cont->nchars + (tail ? tail->nchars:0) + 5;
    int one_line = 0;
    B_str res;
    NEW_UNFILLED_STR(res, res_bytes, res_chars);
    unsigned char *p = res->str;
    *p++ = '<';
    if (prefix) {
        memcpy(p, prefix->str, prefix->nbytes); p += prefix->nbytes;
        *p++ = ':';
    }
    memcpy(p, tag->str, tag->nbytes); p += tag->nbytes;
    memcpy(p, nsdefs->str, nsdefs->nbytes); p += nsdefs->nbytes;
    memcpy(p, attrs->str, attrs->nbytes); p += attrs->nbytes;
    *p++ = '>';
    if (text) {
        memcpy(p, text->str, text->nbytes);
        p += text->nbytes;
    }
    memcpy(p, cont->str, cont->nbytes); p += cont->nbytes;
    *p++ = '<';
    *p++ = '/';
    if (prefix) {
        memcpy(p, prefix->str, prefix->nbytes); p += prefix->nbytes;
        *p++ = ':';
    }
    memcpy(p, tag->str, tag->nbytes); p += tag->nbytes;
    *p++ = '>';
    if (tail) {
        memcpy(p, tail->str, tail->nbytes);
        p += tail->nbytes;
    }
    return res;
}

B_str xmlQ_encode(xmlQ_Node node);

static B_list xmlQ_encode_nodes(B_list nodes) {
    B_list strs = B_listD_new(nodes->length);
    strs->length = nodes->length;
    for (int i=0; i< nodes->length; i++) {
        xmlQ_Node ch = (xmlQ_Node)nodes->data[i];
        strs->data[i] = ch->$class->encode(ch);
    }
    return strs;
}

static B_str xmlQ_encode_nsdefs(B_list nsdefs) {
    int res_bytes = 0;
    int res_chars = 0;
    for (int i=0; i<nsdefs->length;i++) {
        B_tuple nsdef = nsdefs->data[i];
        B_str prefix = (B_str)nsdef->components[0];
        B_str href = (B_str)nsdef->components[1];
        res_bytes += (prefix ? prefix->nbytes+1 : 0) + href->nbytes + 9;
        res_chars += (prefix ? prefix->nchars+1 : 0) + href->nchars + 9;
    }
    B_str res;
    NEW_UNFILLED_STR(res, res_bytes, res_chars);
    unsigned char *p = res->str;
    for (int i=0; i<nsdefs->length; i++) {
        B_tuple nsdef = nsdefs->data[i];
        *p++ = ' ';
        B_str prefix = (B_str)nsdef->components[0];
        B_str href = (B_str)nsdef->components[1];
        char * xmlns = "xmlns";
        memcpy(p, xmlns, 5); p += 5;
        if (prefix) {
            *p++ = ':';
            memcpy(p, prefix->str, prefix->nbytes); p += prefix->nbytes;
        }
        *p++ = '=';
        *p++ = '"';
        memcpy(p, href->str, href->nbytes); p += href->nbytes;
        *p++ = '"';
    }
    return res;
}


static B_str xmlQ_encode_attrs(B_list attrs) {
    int res_bytes = 0;
    int res_chars = 0;
    for (int i=0; i < attrs->length; i++) {
        B_tuple attr = attrs->data[i];
        res_bytes += ((B_str)attr->components[0])->nbytes + ((B_str)attr->components[1])->nbytes + 4; // 4 = ' ','=,'"', '"'
        res_chars += ((B_str)attr->components[0])->nchars + ((B_str)attr->components[1])->nchars + 4;
    }
    B_str res;
    NEW_UNFILLED_STR(res, res_bytes, res_chars);

    unsigned char *p = res->str;
    for (int i=0; i<attrs->length; i++) {
        B_tuple attr = attrs->data[i];
        *p++ = ' ';
        B_str key = (B_str)attr->components[0];
        B_str value = (B_str)attr->components[1];
        memcpy(p, key->str, key->nbytes); p += key->nbytes;
        *p++ = '=';
        *p++ = '"';
        memcpy(p, value->str, value->nbytes); p += value->nbytes;
        *p++ = '"';
    }
    return res;
}

B_str xmlQ_NodeD_encode(xmlQ_Node self) {
    B_str nul = to$str("");
    B_Iterable wit = ((B_Iterable)((B_Collection)B_SequenceD_listG_new()->W_Collection));
    B_list children = xmlQ_encode_nodes(self->children);
    B_str s = nul->$class->join(nul, wit, children);
    B_str nsdefs = xmlQ_encode_nsdefs(self->nsdefs);
    B_str attrs = xmlQ_encode_attrs(self->attributes);
    return xmlQ_node2str(self->tag, nsdefs, self->prefix, attrs, s, self->text, self->tail);
}

void xmlQ___ext_init__() {
    // NOP
}
