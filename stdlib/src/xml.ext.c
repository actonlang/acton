#include <libxml/xmlmemory.h>
#include <libxml/parser.h>

// The macro below is from builtin/str.c. We should not duplicate it...
#define NEW_UNFILLED_STR(nm, nchrs, nbtes)      \
    nm = malloc(sizeof(struct $str));           \
    (nm)->$class = &$str$methods;               \
    (nm)->nchars = nchrs;                       \
    (nm)->nbytes = nbtes;                       \
    (nm)->str = malloc((nm)->nbytes + 1);       \
    (nm)->str[(nm)->nbytes] = 0

xml$$Node $NodePtr2Node(xmlNodePtr node) {
    if (node->type != XML_ELEMENT_NODE) {
        char *errmsg = NULL;
        int ret = asprintf(errmsg, "Unexpected nodetype %d, content is %s", node->type, node->content);
        if (ret == -1)
          $RAISE((($BaseException)$RuntimeError$new(to$str("Unable to format error message"))));
        $RAISE((($BaseException)$RuntimeError$new(to$str(errmsg))));
    }

    $list nsdefs = $list$new(NULL, NULL);
    xmlNsPtr nsDef = node->nsDef;
    while (nsDef) {
        $str prefix = NULL;
        if (nsDef->prefix) prefix = to$str((char *)nsDef->prefix);
        $str href = to$str((char *)nsDef->href);
        $list_append(nsdefs, $NEWTUPLE(2, prefix, href));
        nsDef=nsDef->next;
    }

    $str prefix = NULL;
    if (node->ns)
        prefix = to$str((char *)node->ns->prefix);

    $list attributes = $list$new(NULL, NULL);
    xmlAttrPtr attr = node->properties;
    while (attr) {
        $list_append(attributes, $NEWTUPLE(2, to$str((char *)attr->name), to$str((char *)xmlGetProp(node, attr->name))));
        attr = attr->next;
    }

    $str text = to$str("");
    $str tail = to$str("");

    $list children = $list$new(NULL, NULL);
    xmlNodePtr cur = node->xmlChildrenNode;
    if (cur && cur->type == XML_TEXT_NODE) {
        text = to$str((char *)cur->content);
        cur = cur->next;
    }

    while (cur != NULL) {
        $list_append(children, $NodePtr2Node(cur));
        cur = cur->next;
    }

    if (node->next && node->next->type == XML_TEXT_NODE) { // the root node has no next.
        tail = to$str((char *)node->next->content);
        node->next = node->next->next;
    }
    return (xml$$Node)$NEW(xml$$Node, to$str((char *)node->name), nsdefs, prefix, attributes, children, text, tail);
}

xml$$Node xml$$decode($str data) {
    xmlDocPtr doc = xmlReadMemory((char *)data->str, data->nbytes, NULL, NULL, 0);
    if (!doc) {
        // xmlErrorPtr err = xmlGetLastError();
        $RAISE((($BaseException)$RuntimeError$new(to$str("xml parse error"))));
    }
    xmlNodePtr root = xmlDocGetRootElement(doc);
    xml$$Node t = $NodePtr2Node(root);
    xmlFreeDoc(doc);
    return t;
}


$str xml$$node2str($str tag, $str nsdefs, $str prefix, $str attrs, $str cont, $str text, $str tail) {
    int res_bytes = 2*tag->nbytes + 2*(prefix ? prefix->nbytes+1:0) + nsdefs->nbytes + attrs->nbytes +
                    text->nbytes + cont->nbytes + tail->nbytes + 5; // 5 = len ("<" + ">" + "</" + ">")
    int res_chars = 2*tag->nchars + 2*(prefix ? prefix->nchars+1:0) + nsdefs->nchars + attrs->nchars +
                    text->nchars + cont->nchars + tail->nchars + 5;
    int one_line = 0;
    $str res;
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
    memcpy(p, text->str, text->nbytes); p += text->nbytes;
    memcpy(p, cont->str, cont->nbytes); p += cont->nbytes;
    *p++ = '<';
    *p++ = '/';
    if (prefix) {
        memcpy(p, prefix->str, prefix->nbytes); p += prefix->nbytes;
        *p++ = ':';
    }
    memcpy(p, tag->str, tag->nbytes); p += tag->nbytes;
    *p++ = '>';
    memcpy(p, tail->str, tail->nbytes); p += tail->nbytes;
    return res;
}

$str xml$$encode(xml$$Node node);

static $list xml$$encode_nodes($list nodes) {
    $list strs = $list_new(nodes->length);
    strs->length = nodes->length;
    for (int i=0; i< nodes->length; i++) {
        xml$$Node ch = (xml$$Node)nodes->data[i];
        strs->data[i] = ch->$class->encode(ch);
    }
    return strs;
}

static $str xml$$encode_nsdefs($list nsdefs) {
    int res_bytes = 0;
    int res_chars = 0;
    for (int i=0; i<nsdefs->length;i++) {
        $tuple nsdef = nsdefs->data[i];
        $str prefix = ($str)nsdef->components[0];
        $str href = ($str)nsdef->components[1];
        res_bytes += (prefix ? prefix->nbytes+1 : 0) + href->nbytes + 9;
        res_chars += (prefix ? prefix->nchars+1 : 0) + href->nchars + 9;
    }
    $str res;
    NEW_UNFILLED_STR(res, res_bytes, res_chars);
    unsigned char *p = res->str;
    for (int i=0; i<nsdefs->length; i++) {
        $tuple nsdef = nsdefs->data[i];
        *p++ = ' ';
        $str prefix = ($str)nsdef->components[0];
        $str href = ($str)nsdef->components[1];
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


static $str xml$$encode_attrs($list attrs) {
    int res_bytes = 0;
    int res_chars = 0;
    for (int i=0; i < attrs->length; i++) {
        $tuple attr = attrs->data[i];
        res_bytes += (($str)attr->components[0])->nbytes + (($str)attr->components[1])->nbytes + 4; // 4 = ' ','=,'"', '"'
        res_chars += (($str)attr->components[0])->nchars + (($str)attr->components[1])->nchars + 4;
    }
    $str res;
    NEW_UNFILLED_STR(res, res_bytes, res_chars);

    unsigned char *p = res->str;
    for (int i=0; i<attrs->length; i++) {
        $tuple attr = attrs->data[i];
        *p++ = ' ';
        $str key = ($str)attr->components[0];
        $str value = ($str)attr->components[1];
        memcpy(p, key->str, key->nbytes); p += key->nbytes;
        *p++ = '=';
        *p++ = '"';
        memcpy(p, value->str, value->nbytes); p += value->nbytes;
        *p++ = '"';
    }
    return res;
}

$str xml$$Node$encode(xml$$Node self) {
    $str nul = to$str("");
    $Iterable wit = (($Iterable)(($Collection)$Sequence$list$new()->w$Collection));
    $list children = xml$$encode_nodes(self->children);
    $str s = nul->$class->join(nul, wit, children);
    $str nsdefs = xml$$encode_nsdefs(self->nsdefs);
    $str attrs = xml$$encode_attrs(self->attributes);
    return xml$$node2str(self->tag, nsdefs, self->prefix, attrs, s, self->text, self->tail);
}

void xml$$__ext_init__() {
    // NOP
}
