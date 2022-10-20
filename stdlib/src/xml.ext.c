#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <libxml/xmlmemory.h>
#include <libxml/parser.h>
#include "../out/types/xml.h"

// The macro below is from builtin/str.c. We should not duplicate it...

#define NEW_UNFILLED_STR(nm,nchrs,nbtes)         \
nm = malloc(sizeof(struct $str)); \
(nm)->$class = &$str$methods; \
(nm)->nchars = nchrs;            \
(nm)->nbytes = nbtes;            \
(nm)->str = malloc((nm)->nbytes + 1);    \
(nm)->str[(nm)->nbytes] = 0

<<<<<<< HEAD
 
#define TABSTEP 4
=======
#define INDENTSTEP 4
>>>>>>> b932a168 (improved formatting of encode result (still to be discussed))

xml$$Tag $Doc2Tag(xmlNodePtr root) {
  $Hashable w1 = ($Hashable)$Hashable$str$witness;
  $Mapping w2 = ($Mapping)$Mapping$dict$new(w1);
  $Eq w3 = ($Eq)$Ord$str$witness;
  $Indexed w4 = ($Indexed) $Indexed$dict$new(w2, w3);
  $str name = NULL;
  $list nsdefs = $list_new(0);
  nsdefs->length = 0;
  $list attributes = $list_new(0);
  attributes->length = 0;
  $str content = NULL;
  $str prefix = NULL;
  if (root->ns) prefix = to$str((char *)root->ns->prefix);
  $list children = $list_new(0);
  children->length = 0;
  if (root->type == XML_ELEMENT_NODE) {
     xmlNodePtr cur = root->xmlChildrenNode;
     while (cur != NULL) {
       if (cur->type == XML_ELEMENT_NODE || cur->type == XML_TEXT_NODE ) 
         $list_append(children, $Doc2Tag(cur));
       cur = cur->next;
     }
     name = to$str((char *)root->name);
     xmlAttrPtr attr = root->properties;
     while (attr) {
       $list_append(attributes, $NEWTUPLE(2,to$str((char *)attr->name),to$str((char *)xmlGetProp(root,attr->name))));
       attr = attr->next;
     }
     xmlNsPtr nsDef = root->nsDef;
     while (nsDef) {
       $str prefix = NULL;
       if (nsDef->prefix) prefix =  to$str((char *)nsDef->prefix);
       $str href = to$str((char *)nsDef->href); 
       $list_append(nsdefs, $NEWTUPLE(2,prefix,href));
       nsDef=nsDef->next;
     }
  } else if (root->type == XML_TEXT_NODE) {
    if (strcmp((char *)root->name,"text")) {
      fprintf(stderr,"Internal error: Unknown XML text node");
      exit(1);
    }
    content = to$str((char *)root->content);
  } else {
      fprintf(stderr,"Internal error: Unknown XML node type");
      exit(1);
  }
  return $NEW(xml$$Tag,name,nsdefs,prefix,attributes,content,children);
}

xml$$Tag xml$$decode($str data) {
  xmlDocPtr doc = xmlReadMemory((char *)data->str,data->nbytes,NULL,NULL,XML_PARSE_NOBLANKS);
  if (!doc) {
    // xmlErrorPtr err = xmlGetLastError();
    fprintf(stderr,"xml parse error; quitting\n");
    exit(1);
  }
  xmlNodePtr root = xmlDocGetRootElement(doc);
  xmlNodePtr c = root->children;
  return $Doc2Tag(root);
}


$str xml$$mk_node(int indent, $str tag, $str nsdefs, $str prefix, $str attrs, $str cont) {
  int tag_len = tag->nbytes;
  int cont_len = cont->nbytes;
  int res_bytes = 2*tag->nbytes + 2*(prefix ? prefix->nbytes+1:0) + nsdefs->nbytes + attrs->nbytes + cont->nbytes +2*indent + 7; // 7 = len ("<" + ">" + "\n" + "</" + ">" + "\n")
  int res_chars = 2*tag->nchars + 2*(prefix ? prefix->nchars+1:0) + nsdefs->nchars + attrs->nchars + cont->nchars +2*indent + 7;
<<<<<<< HEAD
<<<<<<< HEAD
=======
>>>>>>> b932a168 (improved formatting of encode result (still to be discussed))
  int one_line = 0;
  if (cont->nbytes<30) {
    one_line = 1;
    for (int i=0; i<cont->nbytes; i++)
      if (cont->str[i] == '\n') {
        one_line = 0;
        break;
      }
  }
  if (one_line) {
    res_bytes -= (2+indent);
    res_chars -= (2+indent);
  }
<<<<<<< HEAD
=======
>>>>>>> a4c946da (first version of xml support)
=======
>>>>>>> b932a168 (improved formatting of encode result (still to be discussed))
  $str res;
  NEW_UNFILLED_STR(res,res_bytes,res_chars);
  unsigned char *p = res->str;
  memset(p,' ',indent); p += indent;
  *p++ = '<';
  if (prefix) {
    memcpy(p,prefix->str,prefix->nbytes); p += prefix->nbytes;
    *p++ = ':';
  }
  memcpy(p, tag->str, tag_len); p += tag_len;
  memcpy(p, nsdefs->str, nsdefs->nbytes); p += nsdefs->nbytes;
  memcpy(p, attrs->str, attrs->nbytes); p += attrs->nbytes;
  *p++ = '>';
  if (!one_line) *p++ = '\n';
  memcpy(p, cont->str, cont_len); p += cont_len;
  if (!one_line) {
    *p++ = '\n';
    memset(p,' ',indent); p += indent;
  }
  *p++ = '<';
  *p++ = '/';
  if (prefix) {
    memcpy(p,prefix->str,prefix->nbytes); p += prefix->nbytes;
    *p++ = ':';
  }
  memcpy(p, tag->str, tag_len); p += tag_len;
  *p++ = '>';  
  return res;
}

static $str xml$$encode_node(int indent, xml$$Tag node);

static $list xml$$encode_nodes(int indent, $list nodes) {
  $list strs = $list_new(nodes->length);
  strs->length = nodes->length;
  for (int i=0; i< nodes->length; i++) 
    strs->data[i] = xml$$encode_node(indent,nodes->data[i]);
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
  NEW_UNFILLED_STR(res,res_bytes,res_chars);
  unsigned char *p = res->str;
  for (int i=0; i<nsdefs->length; i++) {
    $tuple nsdef = nsdefs->data[i];
    *p++ = ' ';
    $str prefix =  ($str)nsdef->components[0];
    $str href =  ($str)nsdef->components[1];
    char * xmlns = "xmlns";
    memcpy(p,xmlns,5); p += 5;
    if (prefix) {
      *p++ = ':';
      memcpy(p,prefix->str,prefix->nbytes); p += prefix->nbytes;
    }
    *p++ = '=';
    *p++ = '"';
    memcpy(p,href->str,href->nbytes); p += href->nbytes;
    *p++ = '"';
  }
  return res;
}
 

static $str xml$$encode_attrs($list attrs) {
  int res_bytes = 0;
  int res_chars = 0;
  for (int i=0; i<attrs->length;i++) {
     $tuple attr = attrs->data[i];
     res_bytes += (($str)attr->components[0])->nbytes +  (($str)attr->components[1])->nbytes + 4; // 4 = ' ','=,'"', '"'
     res_chars +=  (($str)attr->components[0])->nchars +  (($str)attr->components[1])->nchars + 4;
  }
  $str res;
  NEW_UNFILLED_STR(res,res_bytes,res_chars);
  
  unsigned char *p = res->str;
  for (int i=0; i<attrs->length; i++) {
     $tuple attr = attrs->data[i];
    *p++ = ' ';
    $str key =  ($str)attr->components[0];
    $str value =  ($str)attr->components[1];
    memcpy(p,key->str,key->nbytes); p += key->nbytes;
    *p++ = '=';
    *p++ = '"';
    memcpy(p,value->str,value->nbytes); p += value->nbytes;
    *p++ = '"';
  }
  return res;
}

static $str xml$$encode_node(int indent, xml$$Tag node) {
  $str nl = to$str("\n");
  if (node->name) {
    $Iterable wit = (($Iterable)(($Collection)$Sequence$list$new()->w$Collection));
    $list children = xml$$encode_nodes(indent+INDENTSTEP,node->children);
    $str s =  nl->$class->join(nl,wit,children);
    $str nsdefs = xml$$encode_nsdefs(node->nsdefs);
    $str attrs = xml$$encode_attrs(node->attributes);
    return xml$$mk_node(indent,node->name,nsdefs,node->prefix,attrs,s);
  } else {
     return node->content;
  }
}
$str xml$$encode(xml$$Tag root) {
  $str nl = to$str("\n");
  $str res;
  if (root->name) {
    $Iterable wit = (($Iterable)(($Collection)$Sequence$list$new()->w$Collection));
    $str s = nl->$class->join(nl,wit,xml$$encode_nodes(INDENTSTEP,root->children));
    $str nsdefs = xml$$encode_nsdefs(root->nsdefs);
    $str attrs = xml$$encode_attrs(root->attributes);
    res = xml$$mk_node(0,root->name,nsdefs,root->prefix,attrs,s); 
  } else {
    $RAISE(($BaseException)$NEW($ValueError,to$str("xml.encode: No root element")));
  }
  return res;
}

 
void xml$$__ext_init__() {
    // NOP
}
