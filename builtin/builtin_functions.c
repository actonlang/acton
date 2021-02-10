#include "builtin.h"
#include <stdarg.h>

// print //////////////////////////////////////////////////////////////////////////////

static $WORD mkstr($WORD w) {
  $value w1 = ($value)w;
  return w1->$class->__str__(w);
}

void $print(int size, ...) {
    va_list args;
    va_start(args,size);
    if (size > 0) {
        $value elem = va_arg(args,$value);
        fputs((const char*)elem->$class->__str__(elem)->str,stdout);
    }
    for (int i=1; i<size; i++) {
        putchar(' ');
        $value elem = va_arg(args,$value);
        fputs((const char*)elem->$class->__str__(elem)->str,stdout);
    }
    putchar('\n');
    va_end(args);
}

// enumerate //////////////////////////////////////////////////////////////////////////

void $Iterator$enumerate_init($Iterator$enumerate self, $Iterator it, $int n) {
  self->it = it;
  self->nxt = n->val;
}

$bool $Iterator$enumerate_bool($Iterator$enumerate self) {
  return $True;
}

$str $Iterator$enumerate_str($Iterator$enumerate self) {
  char *s;
  asprintf(&s,"<enumerate iterator object at %p>",self);
  return to$str(s);
}

void $Iterator$enumerate_serialize($Iterator$enumerate self,$Serial$state state) {
  $step_serialize(self->it,state);
  $step_serialize(to$int(self->nxt),state);
}

$Iterator$enumerate $Iterator$enumerate$_deserialize($Iterator$enumerate res,$Serial$state state) {
    if (!res)
       res = $DNEW($Iterator$enumerate,state);
   res->it = $step_deserialize(state);
   res->nxt = from$int(($int)$step_deserialize(state));
   return res;
}

$WORD $Iterator$enumerate_next($Iterator$enumerate it) {
  $WORD w = it->it->$class->__next__(it->it);
  if (w)
    return $NEW($tuple,2,to$int(it->nxt++),w);
  else
    return NULL;
}

struct $Iterator$enumerate$class $Iterator$enumerate$methods = {"$Iterator$enumerate",UNASSIGNED,($Super$class)&$Iterator$methods,$Iterator$enumerate_init,
                                                                $Iterator$enumerate_serialize, $Iterator$enumerate$_deserialize, 
                                                                $Iterator$enumerate_bool,$Iterator$enumerate_str, $Iterator$enumerate_next};


$Iterator$enumerate $Iterator$enumerate$new($Iterator it, $int n) {
    return $NEW($Iterator$enumerate, it, n);
}

$Iterator $enumerate($Iterable wit, $WORD iter, $int start) {
  $Iterator it = wit->$class->__iter__(wit,iter);
  if (!start)
    start = to$int(0);
  return ($Iterator)$Iterator$enumerate$new(it,start); 
}

// filter ////////////////////////////////////////////////////////////////////////////////

void $Iterator$filter_init($Iterator$filter self, $Iterator it,  $function f) {
  self->it = it;
  self->f = f;
}

$bool $Iterator$filter_bool($Iterator$filter self) {
  return $True;
}

$str $Iterator$filter_str($Iterator$filter self) {
  char *s;
  asprintf(&s,"<filter iterator object at %p>",self);
  return to$str(s);
}

void $Iterator$filter_serialize($Iterator$filter self,$Serial$state state) {
  $step_serialize(self->it,state);
}

$Iterator$filter $Iterator$filter$_deserialize($Iterator$filter res, $Serial$state state) {
   if (!res)
      res = $DNEW($Iterator$filter,state);
   res->it = $step_deserialize(state);
   return res;
}

$WORD $Iterator$filter_next($Iterator$filter it) {
  $WORD w;
  do
    w = it->it->$class->__next__(it->it);
  while (w && !from$bool(it->f->$class->__call__(it->f, w)));
  return w;
}

struct $Iterator$filter$class $Iterator$filter$methods = {"$Iterator$filter",UNASSIGNED,($Super$class)&$Iterator$methods,$Iterator$filter_init,
                                                          $Iterator$filter_serialize, $Iterator$filter$_deserialize, 
                                                          $Iterator$filter_bool,$Iterator$filter_str, $Iterator$filter_next};

$Iterator$filter $Iterator$filter$new($Iterator it, $function f) {
    return $NEW($Iterator$filter, it, f);
}

$Iterator $filter($Iterable wit, $function f, $WORD iter) {
  $Iterator it = wit->$class->__iter__(wit,iter);
  return ($Iterator)$Iterator$filter$new(it,f);
}

// map ////////////////////////////////////////////////////////////////////////////////

void $Iterator$map_init($Iterator$map self, $Iterator it, $function f) {
  self->it = it;
  self->f = f;
}

$bool $Iterator$map_bool($Iterator$map self) {
  return $True;
}

$str $Iterator$map_str($Iterator$map self) {
  char *s;
  asprintf(&s,"<map iterator object at %p>",self);
  return to$str(s);
}

void $Iterator$map_serialize($Iterator$map self,$Serial$state state) {
  $step_serialize(self->it,state);
}

$Iterator$map $Iterator$map$_deserialize($Iterator$map res, $Serial$state state) {
   if (!res)
      res = $DNEW($Iterator$map,state);
   res->it = $step_deserialize(state);
   return res;
}

$WORD $Iterator$map_next($Iterator$map it) {
  $WORD w = it->it->$class->__next__(it->it);
  if (w)
    return it->f->$class->__call__(it->f, w);
  else
    return NULL;
}

struct $Iterator$map$class $Iterator$map$methods = {"$Iterator$map",UNASSIGNED,($Super$class)&$Iterator$methods,$Iterator$map_init,
                                                                $Iterator$map_serialize, $Iterator$map$_deserialize,  
                                                                $Iterator$map_bool,$Iterator$map_str, $Iterator$map_next};

$Iterator$map $Iterator$map$new($Iterator it, $function f) {
    return $NEW($Iterator$map, it, f);
}

$Iterator $map($Iterable wit, $function f, $WORD iter) {
  $Iterator it = wit->$class->__iter__(wit,iter);
  return ($Iterator)$Iterator$map$new(it,f);
}


// max, min ///////////////////////////////////////////////////////////////////////////////////

$WORD $max($Ord wit, $Iterable wit2, $WORD iter, $WORD deflt) {
  $Iterator it = wit2->$class->__iter__(wit2,iter);  
  $WORD res, nxt;
  res = it->$class->__next__(it);
  if (res) {
    while ((nxt = it->$class->__next__(it))) {
      if (wit->$class->__lt__(wit,res,nxt))
        res = nxt;
    }
    return res;
  } else
    return deflt;
}

$WORD $min($Ord wit, $Iterable wit2, $WORD iter, $WORD deflt) {
  $Iterator it = wit2->$class->__iter__(wit2,iter);  
  $WORD res, nxt;
  res = it->$class->__next__(it);
  if (res) {
    while ((nxt = it->$class->__next__(it))) {
      if (wit->$class->__gt__(wit,res,nxt))
        res = nxt;
    }
    return res;
  } else
    return deflt;
}
 
$list $sorted($Ord wit, $Iterable wit2, $WORD iter) {
  return NULL;
}

// sum /////////////////////////////////////////////////////////////////////////////////

$WORD $sum($Plus wit, $Iterable wit2, $WORD iter, $WORD start) {
  $Iterator it = wit2->$class->__iter__(wit2,iter);  
  $WORD res = start;
  $WORD nxt;
  while ((nxt = it->$class->__next__(it))) 
    res = wit->$class->__add__(wit,res,nxt);
  return res;
}

// zip ////////////////////////////////////////////////////////////////////////////////

void $Iterator$zip_init($Iterator$zip self, $Iterator it1, $Iterator it2) {
  self->it1 = it1;
  self->it2 = it2;
}

$bool $Iterator$zip_bool($Iterator$zip self) {
  return $True;
}

$str $Iterator$zip_str($Iterator$zip self) {
  char *s;
  asprintf(&s,"<zip iterator object at %p>",self);
  return to$str(s);
}

void $Iterator$zip_serialize($Iterator$zip self,$Serial$state state) {
  $step_serialize(self->it1,state);
  $step_serialize(self->it2,state);
}

$Iterator$zip $Iterator$zip$_deserialize($Iterator$zip res, $Serial$state state) {
   if (!res)
      res = $DNEW($Iterator$zip,state);
   res->it1 = $step_deserialize(state);
   res->it2 = $step_deserialize(state);
   return res;
}

$WORD $Iterator$zip_next($Iterator$zip it) {
  $WORD w1 = it->it1->$class->__next__(it->it1);
  $WORD w2 = it->it2->$class->__next__(it->it2);
  if (w1 && w2)
    return $NEW($tuple,2,w1,w2);
  else
    return NULL;
}

struct $Iterator$zip$class $Iterator$zip$methods = {" $Iterator$zip",UNASSIGNED,($Super$class)&$Iterator$methods,$Iterator$zip_init,
                                                    $Iterator$zip_serialize, $Iterator$zip$_deserialize, 
                                                    $Iterator$zip_bool,$Iterator$zip_str, $Iterator$zip_next};

$Iterator$zip $Iterator$zip$new($Iterator iter1, $Iterator iter2) {
    return $NEW($Iterator$zip, iter1, iter2);
}

$Iterator $zip ($Iterable wit1, $Iterable wit2, $WORD iter1, $WORD iter2) {
  $Iterator it1 = wit1->$class->__iter__(wit1,iter1);
  $Iterator it2 = wit2->$class->__iter__(wit2,iter2);
  return ($Iterator)$Iterator$zip$new(it1,it2);
}

// EqOpt //////////////////////////////////////////////////////

void $EqOpt$__init__($EqOpt wit, $Eq w$Eq$A) {
    wit->w$Eq$A = w$Eq$A;
}

$bool $EqOpt$__eq__($EqOpt wit, $WORD a, $WORD b) {
    if (a && b)
        return wit->w$Eq$A->$class->__eq__(wit->w$Eq$A, a, b);
    return (!a && !b) ? $True : $False;
}

$bool $EqOpt$__ne__($EqOpt wit, $WORD a, $WORD b) {
    if (a && b)
        return wit->w$Eq$A->$class->__ne__(wit->w$Eq$A, a, b);
    return (!a && !b) ? $False : $True;
}

struct $EqOpt$class $EqOpt$methods = {"$EqOpt", UNASSIGNED, NULL, $EqOpt$__init__, $EqOpt$__eq__, $EqOpt$__ne__};


$EqOpt $EqOpt$new($Eq w$Eq$A) {
    return $NEW($EqOpt, w$Eq$A);
}


// Various small functions //////////////////////////////////////////////////////////////

// Code generated by actonc

$WORD $abs ($Number w$149, $Real w$148, $WORD x) {
    return w$149->$class->__abs__(w$149, x, w$148);
}
$bool $all ($Iterable w$164, $WORD it) {
    $Iterator n$iter = w$164->$class->__iter__(w$164, it);
    $WORD n$1val = n$iter->$class->__next__(n$iter);
    while ($ISNOTNONE(n$1val)) {
        $value x = ($value)n$1val;
        if (!x->$class->__bool__(x)->val) {
            return ($bool)$False;
        }
        n$1val = n$iter->$class->__next__(n$iter);
    }
    return ($bool)$True;
}
$bool $any ($Iterable w$179, $WORD it) {
    $Iterator n$2iter = w$179->$class->__iter__(w$179, it);
    $WORD n$3val = n$2iter->$class->__next__(n$2iter);
    while ($ISNOTNONE(n$3val)) {
        $value x = ($value)n$3val;
        if (x->$class->__bool__(x)->val) {
            return ($bool)$True;
        }
        n$3val = n$2iter->$class->__next__(n$2iter);
    }
    return ($bool)$False;
}
$tuple $divmod ($Integral w$225, $WORD a, $WORD b) {
    return w$225->$class->__divmod__(w$225, a, b);
}
$int $hash ($Hashable w$255, $WORD x) {
    return w$255->$class->__hash__(w$255, x);
}
$Iterator $iter ($Iterable w$278, $WORD x) {
    return w$278->$class->__iter__(w$278, x);
}
$int $len ($Collection w$301, $WORD x) {
    return w$301->$class->__len__(w$301, x);
}
$WORD $pow ($Number w$344, $WORD a, $WORD b) {
    return w$344->$class->__pow__(w$344, a, b);
}
$Iterator $reversed ($Sequence w$369, $WORD seq) {
    return w$369->$class->__reversed__(w$369, seq);
}
$WORD $round ($Real w$395, $WORD x, $int n) {
    return w$395->$class->__round__(w$395, x, n);
}


$list $replicate($int n, $WORD elem) {
  $list res = $list_new(n->val);
  memset_pattern8(res->data,&elem,8*n->val);
  res->length = n->val;
  return res;
}
