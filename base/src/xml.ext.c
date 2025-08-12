#define LIBXML_STATIC
#include <libxml/xmlmemory.h>
#include <libxml/parser.h>

// TODO: The macro below is from builtin/str.c. We should not duplicate it...
#define NEW_UNFILLED_STR(nm, nchrs, nbtes)      \
    assert(nbtes >= nchrs);                     \
    nm = acton_malloc(sizeof(struct B_str));           \
    (nm)->$class = &B_strG_methods;               \
    (nm)->nchars = nchrs;                       \
    (nm)->nbytes = nbtes;                       \
    (nm)->str = acton_malloc_atomic((nm)->nbytes + 1);       \
    (nm)->str[(nm)->nbytes] = 0

// Helper function to count extra bytes needed for XML escaping
// Returns the number of extra bytes needed to replace the special character
// with encoded entity (not total bytes)
//
static int count_xml_escape_extra(B_str str, int escape_quotes) {
    int extra = 0;
    // Note: It's safe to iterate byte-by-byte even for UTF-8 strings because we
    // only check for ASCII characters (&, <, "). The codepoints for ASCII
    // characters are backwards compatible (0x00-0x7F -> 00000000-01111111).
    // Continuation bytes (2nd, ...) in multi-byte UTF-8 characters always have
    // the same pattern 10xxxxxx (0x80-0xBF) so no UTF-8 continuation byte can
    // be mistaken for an ASCII character.
    for (int i = 0; i < str->nbytes; i++) {
        switch (str->str[i]) {
            case '&': extra += 4; break;  // &amp; = 5 bytes instead of 1
            case '<': extra += 3; break;  // &lt; = 4 bytes instead of 1
            case '"':
                if (escape_quotes) extra += 5;  // &quot; = 6 bytes instead of 1
                break;
        }
    }
    return extra;
}

// Helper function to copy string with XML escaping
// Returns pointer to position after copied data
static unsigned char* copy_with_xml_escape(unsigned char *dst, B_str src, int escape_quotes) {
    for (int i = 0; i < src->nbytes; i++) {
        // Note: It's safe to iterate byte-for-byte here because we're only
        // inserting ASCII and copying (maybe UTF-8 multi-byte) characters as
        // individual bytes, iterating over the total byte length of B_str
        switch (src->str[i]) {
            case '&':
                memcpy(dst, "&amp;", 5);
                dst += 5;
                break;
            case '<':
                memcpy(dst, "&lt;", 4);
                dst += 4;
                break;
            case '"':
                if (escape_quotes) {
                    memcpy(dst, "&quot;", 6);
                    dst += 6;
                } else {
                    *dst++ = src->str[i];
                }
                break;
            default:
                *dst++ = src->str[i];
                break;
        }
    }
    return dst;
}

xmlQ_Node $NodePtr2Node(xmlNodePtr node) {
    B_SequenceD_list wit = B_SequenceD_listG_witness;
    if (node->type == XML_COMMENT_NODE) {
        return NULL;
    }
    if (node->type != XML_ELEMENT_NODE) {
        char *errmsg = NULL;
        RAISE(xmlQ_XmlParseError, $FORMAT("Unexpected nodetype %d, content is %s", node->type, node->content), NULL, NULL);
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
        xmlQ_Node child = $NodePtr2Node(cur);
        if (child)
            wit->$class->append(wit,children, child);
        cur = cur->next;
    }

    if (node->next && node->next->type == XML_TEXT_NODE) { // the root node has no next.
        tail = to$str((char *)node->next->content);
        node->next = node->next->next;
    }
    return (xmlQ_Node)$NEW(xmlQ_Node, to$str((char *)node->name), nsdefs, prefix, attributes, children, text, tail);
}

xmlQ_Node xmlQ_decode(B_str data) {
    // With XML_PARSE_NOERROR we suppress printing error and warning reports to stderr
    xmlDocPtr doc = xmlReadMemory((char *)data->str, data->nbytes, NULL, NULL, XML_PARSE_NOERROR);
    if (!doc) {
        xmlErrorPtr err = xmlGetLastError();
        B_str errmsg;
        B_int line = NULL;
        B_int column = NULL;

        if (err && err->message) {
            if (err->line > 0) {
                line = to$int(err->line);
            }
            if (err->int2 > 0) {  // int2 contains the column in libxml2
                column = to$int(err->int2);
            }

            // Strip trailing whitespace from error message if needed
            int orig_len = strlen(err->message);
            int len = orig_len;
            while (len > 0 && isspace((unsigned char)err->message[len-1])) {
                len--;
            }

            if (len < orig_len) {
                // Only copy if we actually stripped something
                char msg_clean[len + 1];
                strncpy(msg_clean, err->message, len);
                msg_clean[len] = '\0';
                errmsg = to$str(msg_clean);
            } else {
                // Use original message as-is
                errmsg = to$str(err->message);
            }
        } else {
            errmsg = to$str("XML parse error");
        }
        RAISE(xmlQ_XmlParseError, errmsg, line, column);
    }
    xmlNodePtr root = xmlDocGetRootElement(doc);
    xmlQ_Node t = $NodePtr2Node(root);
    xmlFreeDoc(doc);
    return t;
}


B_str xmlQ_node2str(B_str tag, B_str nsdefs, B_str prefix, B_str attrs, B_str cont, B_str text, B_str tail) {
    // Calculate extra bytes needed for escaping text and tail
    int text_extra = text ? count_xml_escape_extra(text, 0) : 0;
    int tail_extra = tail ? count_xml_escape_extra(tail, 0) : 0;

    int res_bytes = 2*tag->nbytes + 2*(prefix ? prefix->nbytes+1:0) + nsdefs->nbytes + attrs->nbytes +
                    (text ? text->nbytes + text_extra : 0) + cont->nbytes +
                    (tail ? tail->nbytes + tail_extra : 0) + 5; // 5 = len("<" + ">" + "</" + ">")
    int res_chars = 2*tag->nchars + 2*(prefix ? prefix->nchars+1:0) + nsdefs->nchars + attrs->nchars +
                    (text ? text->nchars + text_extra : 0) + cont->nchars +
                    (tail ? tail->nchars + tail_extra : 0) + 5;

    B_str res;
    NEW_UNFILLED_STR(res, res_chars, res_bytes);
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
        p = copy_with_xml_escape(p, text, 0);
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
        p = copy_with_xml_escape(p, tail, 0);
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

        // Count extra bytes needed for escaping href
        int href_extra = count_xml_escape_extra(href, 1);

        res_bytes += (prefix ? prefix->nbytes+1 : 0) + href->nbytes + href_extra + 9; // 9 = len(" xmlns" + "=" + '"' + '"')
        res_chars += (prefix ? prefix->nchars+1 : 0) + href->nchars + href_extra + 9;
    }
    B_str res;
    NEW_UNFILLED_STR(res, res_chars, res_bytes);
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
        p = copy_with_xml_escape(p, href, 1);
        *p++ = '"';
    }
    return res;
}


static B_str xmlQ_encode_attrs(B_list attrs) {
    int res_bytes = 0;
    int res_chars = 0;
    for (int i=0; i < attrs->length; i++) {
        B_tuple attr = attrs->data[i];
        B_str key = (B_str)attr->components[0];
        B_str value = (B_str)attr->components[1];

        // Count extra bytes needed for escaping
        int extra_bytes = count_xml_escape_extra(value, 1);

        res_bytes += key->nbytes + value->nbytes + extra_bytes + 4; // 4 = len(" " + "=" + "'" + "'")
        res_chars += key->nchars + value->nchars + extra_bytes + 4;
    }
    B_str res;
    NEW_UNFILLED_STR(res, res_chars, res_bytes);

    unsigned char *p = res->str;
    for (int i=0; i < attrs->length; i++) {
        B_tuple attr = attrs->data[i];
        B_str key = (B_str)attr->components[0];
        B_str value = (B_str)attr->components[1];
        *p++ = ' ';
        memcpy(p, key->str, key->nbytes); p += key->nbytes;
        *p++ = '=';
        *p++ = '"';
        p = copy_with_xml_escape(p, value, 1);
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
