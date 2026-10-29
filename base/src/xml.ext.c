#define LIBXML_STATIC
#include <libxml/xmlmemory.h>
#include <libxml/parser.h>

static struct xmlQ_XmlParseError xmlQ_no_root_error =
    STATIC_EXCEPTION(xmlQ_XmlParseError, "Document has no root element");
static struct xmlQ_XmlParseError xmlQ_parse_error =
    STATIC_EXCEPTION(xmlQ_XmlParseError, "XML parse error");
static struct B_MemoryError xmlQ_allocation_failed_error =
    STATIC_EXCEPTION(B_MemoryError, "memory allocation failed");

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
static int64_t count_xml_escape_extra(B_str str, int escape_quotes) {
    int64_t extra = 0;
    // Note: It's safe to iterate byte-by-byte even for UTF-8 strings because we
    // only check for ASCII characters (&, <, "). The codepoints for ASCII
    // characters are backwards compatible (0x00-0x7F -> 00000000-01111111).
    // Continuation bytes (2nd, ...) in multi-byte UTF-8 characters always have
    // the same pattern 10xxxxxx (0x80-0xBF) so no UTF-8 continuation byte can
    // be mistaken for an ASCII character.
    for (int64_t i = 0; i < str->nbytes; i++) {
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
    for (int64_t i = 0; i < src->nbytes; i++) {
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

// Collect character data (text and CDATA) starting at *cur_ptr, up to but not
// including the next element node. Comments are skipped over transparently, so
// text on either side of a comment is concatenated (including whitespace).
// Unsupported node types raise XmlParseError.
// On return *cur_ptr points at the next element node, or NULL if the siblings
// are exhausted; callers can therefore resume iterating element children
// directly.
//
// Returns the combined text, or NULL if there was no character data (only
// comments or nothing at all).
//
// Note: cur_ptr is passed by reference (pointer to pointer) so we can advance
//       the caller's cursor past everything we consumed.
static B_str collect_text_cdata_nodes(xmlNodePtr *cur_ptr) {
    // First pass: sum the length of all text/CDATA content and locate the
    // element node (or end of siblings) where collection stops.
    size_t text_len = 0;
    xmlNodePtr cur = *cur_ptr;
    while (cur && cur->type != XML_ELEMENT_NODE) {
        switch (cur->type) {
            case XML_TEXT_NODE:
            case XML_CDATA_SECTION_NODE:
                if (cur->content)
                    text_len += strlen((char *)cur->content);
                break;
            case XML_COMMENT_NODE:
                break;
            default:
                RAISE(xmlQ_XmlParseError, $FORMAT("Unsupported XML node type %d", cur->type), NULL, NULL);
                return NULL;
        }
        cur = cur->next;
    }
    xmlNodePtr stop = cur;

    if (text_len == 0) {
        *cur_ptr = stop;
        return NULL;
    }

    // Second pass: concatenate the text/CDATA content into a single string.
    char *combined = acton_malloc_atomic(text_len + 1);
    if (!combined)
        RAISE_EXC(&xmlQ_allocation_failed_error);
    char *p = combined;
    for (cur = *cur_ptr; cur != stop; cur = cur->next) {
        if ((cur->type == XML_TEXT_NODE || cur->type == XML_CDATA_SECTION_NODE) && cur->content) {
            size_t len = strlen((char *)cur->content);
            memcpy(p, cur->content, len);
            p += len;
        }
    }
    *p = '\0';

    *cur_ptr = stop;
    return actStrFromCString(combined);
}

// Convert a libxml2 element node into an Acton xml.Node.
//
// The returned Node has tail == NULL. An element's tail (the character data
// following it, up to its next sibling element) belongs to the parent's child
// sequence, so it is filled in by the caller while iterating siblings.
//
// Raises XmlParseError if the node or one of its descendants cannot be
// represented. libxml2 copies each attribute value with malloc. The copy is
// kept in *value while it is converted, so that the exception handler of
// xmlQ_decode frees it when the conversion raises.
xmlQ_Node $NodePtr2Node(xmlNodePtr node, xmlChar *volatile *value) {
    B_SequenceD_list wit = B_SequenceD_listG_witness;
    if (node->type != XML_ELEMENT_NODE) {
        RAISE(xmlQ_XmlParseError, $FORMAT("Unexpected nodetype %d, content is %s", node->type, node->content), NULL, NULL);
        return NULL;
    }

    B_list nsdefs = B_listG_new(NULL, NULL);
    xmlNsPtr nsDef = node->nsDef;
    while (nsDef) {
        B_str prefix = NULL;
        if (nsDef->prefix) prefix = actStrFromCStringCopy((const char *)nsDef->prefix);
        B_str href = actStrFromCStringCopy((const char *)nsDef->href);
        wit->$class->append(wit,nsdefs, $NEWTUPLE(2, prefix, href));
        nsDef=nsDef->next;
    }

    B_str prefix = NULL;
    if (node->ns && node->ns->prefix)
        prefix = actStrFromCStringCopy((const char *)node->ns->prefix);

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
            attr_name = actStrFromCStringCopy((const char *)attr->name);
        }
        // The attribute exists, so xmlGetProp returns NULL only when malloc
        // fails.
        *value = xmlGetProp(node, attr->name);
        if (!*value)
            RAISE_EXC(&xmlQ_allocation_failed_error);
        B_str value_str = actStrFromCStringCopy((const char *)*value);
        xmlFree(*value);
        *value = NULL;
        wit->$class->append(wit,attributes, $NEWTUPLE(2, attr_name, value_str));
        attr = attr->next;
    }

    B_list children = B_listG_new(NULL, NULL);
    xmlNodePtr cur = node->xmlChildrenNode;

    // Character data before the first child element becomes this node's text.
    B_str text = collect_text_cdata_nodes(&cur);

    // collect_text_cdata_nodes stops only at element nodes (cur is updated), so
    // every node seen here is an element. Recurse into it, then collect the
    // character data that follows it (up to the next element) as that child's
    // tail.
    while (cur != NULL) {
        xmlQ_Node child = $NodePtr2Node(cur, value);
        cur = cur->next;
        child->tail = collect_text_cdata_nodes(&cur);
        wit->$class->append(wit, children, child);
    }

    return (xmlQ_Node)$NEW(xmlQ_Node, actStrFromCStringCopy((const char *)node->name), nsdefs, prefix, attributes, children, text, NULL);
}

xmlQ_Node xmlQ_decode(B_str data) {
    // libxml2 allocates with malloc/free. ctxt, doc and value are assigned
    // after the setjmp of $PUSH and read in the handler, thus volatile. The
    // frees accept NULL.
    xmlParserCtxtPtr volatile ctxt = NULL;
    xmlDocPtr volatile doc = NULL;
    xmlChar *volatile value = NULL;
    xmlQ_Node res = NULL;
    struct JumpBuf frame;
    if ($PUSH(&frame)) {
        // Parse with a context of our own and take a failure's error from it:
        // the context holds this parse's last diagnostic, whatever else libxml2
        // does. For an empty document that is "Document is empty".
        // XML_PARSE_NO_GLOBAL_ERROR keeps errors out of libxml2's global last
        // error record, so parsing writes no state shared between threads.
        ctxt = xmlNewParserCtxt();
        if (!ctxt)
            RAISE_EXC(&xmlQ_parse_error);
        // With XML_PARSE_NOERROR we suppress printing error and warning reports to stderr
        // Support CDATA sections larger than libxml2's default 10 MB limit.
        // XML_PARSE_HUGE raises the limit to 1 GB.
        xmlCtxtUseOptions(ctxt, XML_PARSE_NOERROR | XML_PARSE_NO_GLOBAL_ERROR | XML_PARSE_HUGE);
        // Parse the str's bytes where they are instead of copying them: str
        // memory never changes and ends in a NUL byte. libxml2 keeps the only
        // pointer to it in memory the GC doesn't scan, so data must stay
        // reachable until parsing is done.
        xmlParserInputFlags flags = XML_INPUT_BUF_STATIC;
        if (data->str[data->nbytes] == 0)
            flags |= XML_INPUT_BUF_ZERO_TERMINATED;
        xmlParserInputPtr input = xmlNewInputFromMemory(NULL, data->str, data->nbytes, flags);
        doc = xmlCtxtParseDocument(ctxt, input);
        GC_reachable_here(data);
        if (!doc) {
            const xmlError *err = xmlCtxtGetLastError(ctxt);
            if (!err || !err->message)
                RAISE_EXC(&xmlQ_parse_error);
            B_int line = NULL;
            B_int column = NULL;
            if (err->line > 0) {
                line = toB_int(err->line);
            }
            if (err->int2 > 0) {  // int2 contains the column in libxml2
                column = toB_int(err->int2);
            }
            // Strip trailing whitespace from the error message
            size_t len = strlen(err->message);
            while (len > 0 && isspace((unsigned char)err->message[len-1])) {
                len--;
            }
            RAISE(xmlQ_XmlParseError, actStrFromCStringLengthCopy(err->message, len), line, column);
        }
        xmlNodePtr root = xmlDocGetRootElement(doc);
        if (!root)
            RAISE_EXC(&xmlQ_no_root_error);
        res = $NodePtr2Node(root, &value);
        $DROP();
    } else {
        B_BaseException ex = $POP();
        xmlFree(value);
        xmlFreeDoc(doc);
        xmlFreeParserCtxt(ctxt);
        $RAISE(ex);
    }
    xmlFreeDoc(doc);
    xmlFreeParserCtxt(ctxt);
    return res;
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
    B_str nul = actStrFromCString("");
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
            B_str separator = actStrFromCString("\n");
            children_str = separator->$class->join(separator, (B_Iterable)B_SequenceD_listG_witness->W_Collection, children);
        } else {
            // Join with empty string
            children_str = nul->$class->join(nul, (B_Iterable)B_SequenceD_listG_witness->W_Collection, children);
        }
    } else {
      children_str = nul;
    }

    // Calculate extra bytes needed for escaping text and tail
    int64_t text_extra = node->text ? count_xml_escape_extra(node->text, 0) : 0;
    int64_t tail_extra = node->tail ? count_xml_escape_extra(node->tail, 0) : 0;

    int indent_size = pretty ? depth * 2 : 0;

    // Calculate total size
    int64_t res_bytes = indent_size +
                    (is_empty ? 1 : 2) * (node->tag->nbytes + (node->prefix ? node->prefix->nbytes + 1 : 0)) +
                    nsdefs->nbytes + attrs->nbytes +
                    (is_empty ? 0 : (node->text ? node->text->nbytes + text_extra : 0)) +
                    (is_empty ? 0 : (has_children && pretty ? 1 : 0)) + // newline before children
                    (is_empty ? 0 : children_str->nbytes) +
                    (is_empty ? 0 : (has_children && pretty ? 1 + indent_size : 0)) + // newline + indent before closing
                    (node->tail ? node->tail->nbytes + tail_extra : 0) +
                    (is_empty ? 3 : 5); // self-closing tag - 3 bytes (< / >); open+close tag - 5 bytes (< > < / >)

    int64_t res_chars = indent_size +
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
    int64_t res_bytes = 0;
    int64_t res_chars = 0;
    for (int i=0; i<nsdefs->length;i++) {
        B_tuple nsdef = nsdefs->data[i];
        B_str prefix = (B_str)nsdef->components[0];
        B_str href = (B_str)nsdef->components[1];

        // Count extra bytes needed for escaping href
        int64_t href_extra = count_xml_escape_extra(href, 1);

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
    int64_t res_bytes = 0;
    int64_t res_chars = 0;
    for (int i=0; i < attrs->length; i++) {
        B_tuple attr = attrs->data[i];
        B_str key = (B_str)attr->components[0];
        B_str value = (B_str)attr->components[1];

        // Count extra bytes needed for escaping
        int64_t extra_bytes = count_xml_escape_extra(value, 1);

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

B_str xmlQ_NodeD_encode(xmlQ_Node self, bool pretty) {
    // Use the internal function with depth 0 for the root node
    return xmlQ_node2str(self, pretty, 0);
}

void xmlQ___ext_init__() {
    // libxml2 is built without thread support, so its one-time
    // initialisation is not locked. Module init runs before any worker
    // thread parses.
    xmlInitParser();
}
