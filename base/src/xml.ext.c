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

// Helper function to collect text from consecutive TEXT and CDATA nodes
// Returns the combined string and updates the node pointer to the first non-text node
// Note: cur_ptr is passed by reference (pointer to pointer) so we can update the caller's pointer
//       to skip past all consumed text/CDATA nodes
static B_str collect_text_cdata_nodes(xmlNodePtr *cur_ptr) {
    xmlNodePtr cur = *cur_ptr;
    if (!cur || (cur->type != XML_TEXT_NODE && cur->type != XML_CDATA_SECTION_NODE)) {
        return NULL;
    }

    // Count total length of combined text and CDATA nodes
    size_t text_len = 0;
    xmlNodePtr text_start = cur;
    while (cur && (cur->type == XML_TEXT_NODE || cur->type == XML_CDATA_SECTION_NODE)) {
        if (cur->content) text_len += strlen((char *)cur->content);
        cur = cur->next;
    }

    // Create combined string
    if (text_len > 0) {
        char *combined = acton_malloc_atomic(text_len + 1);
        char *p = combined;
        xmlNodePtr t = text_start;
        while (t != cur) {
            if (t->content) {
                size_t len = strlen((char *)t->content);
                memcpy(p, t->content, len);
                p += len;
            }
            t = t->next;
        }
        *p = '\0';
        B_str result = to_str_noc(combined);
        *cur_ptr = cur;
        return result;
    }

    *cur_ptr = cur;
    return NULL;
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
        B_str attr_name;
        // libxml2 handles namespace prefixes in two ways:
        // 1. Defined namespaces: attr->ns is set, attr->name has local name only
        //    e.g., xmlns:ns="http://foo" ns:op="x" -> attr->ns->prefix="ns", attr->name="op"
        // 2. Undefined namespaces: attr->ns is NULL, attr->name contains the full prefixed name
        //    e.g., ns:op="x" (no xmlns:ns) -> attr->ns=NULL, attr->name="ns:op"
        if (attr->ns && attr->ns->prefix) {
            // Reconstruct prefixed name for defined namespace
            attr_name = $FORMAT("%s:%s", attr->ns->prefix, attr->name);
        } else {
            // Use name as-is (either unprefixed or undefined prefix already in name)
            attr_name = to$str((char *)attr->name);
        }
        wit->$class->append(wit,attributes, $NEWTUPLE(2, attr_name, to$str((char *)xmlGetProp(node, attr->name))));
        attr = attr->next;
    }

    B_list children = B_listG_new(NULL, NULL);
    xmlNodePtr cur = node->xmlChildrenNode;

    // Collect initial text/CDATA nodes
    B_str text = collect_text_cdata_nodes(&cur);

    while (cur != NULL) {
        xmlQ_Node child = $NodePtr2Node(cur);
        if (child)
            wit->$class->append(wit,children, child);
        cur = cur->next;
    }

    // Collect tail text/CDATA nodes after we have exhausted the child nodes
    cur = node->next;
    B_str tail = collect_text_cdata_nodes(&cur);
    // Update the tree structure to skip consumed tail text/CDATA nodes.
    // This prevents the parent from seeing these text nodes again during its
    // child iteration, since tail text of an element is part of the parent's
    // child list in the XML tree.
    node->next = cur;
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


static B_str xmlQ_encode_nsdefs(B_list nsdefs);
static B_str xmlQ_encode_attrs(B_list attrs);


B_str xmlQ_node2str(xmlQ_Node node, bool pretty, int depth) {
    B_str nsdefs = xmlQ_encode_nsdefs(node->nsdefs);
    B_str attrs = xmlQ_encode_attrs(node->attributes);

    // Is this an empty element (no text, no children), then use self-closing tag
    bool is_empty = !node->text && node->children->length == 0;


    // Encode the child nodes to a single string (pretty-printed)
    bool has_children = node->children->length > 0;
    B_str nul = to$str("");
    B_str children_str;

    if (has_children) {
        B_list children = B_listD_new(node->children->length);
        children->length = node->children->length;
        for (int i = 0; i < node->children->length; i++) {
            xmlQ_Node ch = (xmlQ_Node)node->children->data[i];
            children->data[i] = xmlQ_node2str(ch, pretty, depth + 1);
        }

        if (pretty) {
            // Join with newlines
            B_str separator = to$str("\n");
            children_str = separator->$class->join(separator, B_SequenceD_listG_witness->W_Collection, children);
        } else {
            // Join with empty string
            children_str = nul->$class->join(nul, B_SequenceD_listG_witness->W_Collection, children);
        }
    } else {
      children_str = nul;
    }

    // Calculate extra bytes needed for escaping text and tail
    int text_extra = node->text ? count_xml_escape_extra(node->text, 0) : 0;
    int tail_extra = node->tail ? count_xml_escape_extra(node->tail, 0) : 0;

    int indent_size = pretty ? depth * 2 : 0;

    // Calculate total size
    int res_bytes = indent_size +
                    (is_empty ? 1 : 2) * (node->tag->nbytes + (node->prefix ? node->prefix->nbytes + 1 : 0)) +
                    nsdefs->nbytes + attrs->nbytes +
                    (is_empty ? 0 : (node->text ? node->text->nbytes + text_extra : 0)) +
                    (is_empty ? 0 : (has_children && pretty ? 1 : 0)) + // newline before children
                    (is_empty ? 0 : children_str->nbytes) +
                    (is_empty ? 0 : (has_children && pretty ? 1 + indent_size : 0)) + // newline + indent before closing
                    (node->tail ? node->tail->nbytes + tail_extra : 0) +
                    (is_empty ? 3 : 5); // self-closing tag - 3 bytes (< / >); open+close tag - 5 bytes (< > < / >)

    int res_chars = indent_size +
                    (is_empty ? 1 : 2) * (node->tag->nchars + (node->prefix ? node->prefix->nchars + 1 : 0)) +
                    nsdefs->nchars + attrs->nchars +
                    (is_empty ? 0 : (node->text ? node->text->nchars + text_extra : 0)) +
                    (is_empty ? 0 : (has_children && pretty ? 1 : 0)) +
                    (is_empty ? 0 : children_str->nchars) +
                    (is_empty ? 0 : (has_children && pretty ? 1 + indent_size : 0)) +
                    (node->tail ? node->tail->nchars + tail_extra : 0) +
                    (is_empty ? 3 : 5);

    // Build the result string
    B_str res;
    NEW_UNFILLED_STR(res, res_chars, res_bytes);
    unsigned char *p = res->str;

    // Write initial indentation
    for (int i = 0; i < indent_size; i++) {
        *p++ = ' ';
    }

    // Write opening tag
    *p++ = '<';
    if (node->prefix) {
        memcpy(p, node->prefix->str, node->prefix->nbytes);
        p += node->prefix->nbytes;
        *p++ = ':';
    }
    memcpy(p, node->tag->str, node->tag->nbytes);
    p += node->tag->nbytes;
    memcpy(p, nsdefs->str, nsdefs->nbytes);
    p += nsdefs->nbytes;
    memcpy(p, attrs->str, attrs->nbytes);
    p += attrs->nbytes;

    // Write self-closing tag or children followed by explicit close tag
    if (is_empty) {
        *p++ = '/';
        *p++ = '>';
    } else {
        *p++ = '>';

        // Write text content
        if (node->text) {
            p = copy_with_xml_escape(p, node->text, 0);
        }

        if (has_children) {
            // Write newline after opening tag, write children
            if (pretty) {
                *p++ = '\n';
            }
            memcpy(p, children_str->str, children_str->nbytes);
            p += children_str->nbytes;

            // Write newline and indent before closing tag if needed
            if (pretty) {
                *p++ = '\n';
                for (int i = 0; i < indent_size; i++) {
                    *p++ = ' ';
                }
            }
        }

        // Write closing tag
        *p++ = '<';
        *p++ = '/';
        if (node->prefix) {
            memcpy(p, node->prefix->str, node->prefix->nbytes);
            p += node->prefix->nbytes;
            *p++ = ':';
        }
        memcpy(p, node->tag->str, node->tag->nbytes);
        p += node->tag->nbytes;
        *p++ = '>';
    }

    // Write tail
    if (node->tail) {
        p = copy_with_xml_escape(p, node->tail, 0);
    }

    return res;
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

B_str xmlQ_NodeD_encode(xmlQ_Node self, B_bool pretty) {
    // Use the internal function with depth 0 for the root node
    return xmlQ_node2str(self, pretty ? pretty->val != 0 : false, 0);
}

void xmlQ___ext_init__() {
    // NOP
}
