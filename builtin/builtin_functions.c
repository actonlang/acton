#include "builtin.h"
#include <stdarg.h>

// print //////////////////////////////////////////////////////////////////////////////

static $WORD mkstr($WORD w) {
  $struct w1 = ($struct)w;
  return w1->$class->__str__(w);
}

void $print(int size, ...) {
    va_list args;
    va_start(args,size);
    if (size > 0) {
        $struct elem = va_arg(args,$struct);
        puts((const char*)elem->$class->__str__(elem)->str);
    }
    for (int i=1; i<size; i++) {
        putchar(' ');
        $struct elem = va_arg(args,$struct);
        puts((const char*)elem->$class->__str__(elem)->str);
    }
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

$Iterator$enumerate $Iterator$enumerate$_deserialize($Serial$state state) {
   $Iterator$enumerate res = $DNEW($Iterator$enumerate,state);
   res->it = $step_deserialize(state);
   res->nxt = from$int(($int)$step_deserialize(state));
   return res;
}

$WORD $Iterator$enumerate_next($Iterator$enumerate it) {
  $WORD w = it->it->$class->__next__(it->it);
  if (w)
    return $NEW($tuple,2,w,to$int(it->nxt++));
  else
    return NULL;
}

struct $Iterator$enumerate$class $Iterator$enumerate$methods = {"",UNASSIGNED,($Super$class)&$Iterator$methods,$Iterator$enumerate_init,
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

void $Iterator$filter_init($Iterator$filter self, $Iterator it, $bool(*f)($WORD)) {
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

$Iterator$filter $Iterator$filter$_deserialize($Serial$state state) {
   $Iterator$filter res = $DNEW($Iterator$filter,state);
   res->it = $step_deserialize(state);
   return res;
}

$WORD $Iterator$filter_next($Iterator$filter it) {
  $WORD w;
  do
    w = it->it->$class->__next__(it->it);
  while (w && !from$bool(it->f(w)));
  return w;
}

struct $Iterator$filter$class $Iterator$filter$methods = {"",UNASSIGNED,($Super$class)&$Iterator$methods,$Iterator$filter_init,
                                                          $Iterator$filter_serialize, $Iterator$filter$_deserialize, 
                                                          $Iterator$filter_bool,$Iterator$filter_str, $Iterator$filter_next};

$Iterator$filter $Iterator$filter$new($Iterator it, $bool(*f)($WORD)) {
    return $NEW($Iterator$filter, it, f);
}

$Iterator $filter($Iterable wit, $bool(*f)($WORD), $WORD iter) {
  $Iterator it = wit->$class->__iter__(wit,iter);
  return ($Iterator)$Iterator$filter$new(it,f);
}

// map ////////////////////////////////////////////////////////////////////////////////

void $Iterator$map_init($Iterator$map self, $Iterator it, $WORD(*f)($WORD)) {
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

$Iterator$map $Iterator$map$_deserialize($Serial$state state) {
   $Iterator$map res = $DNEW($Iterator$map,state);
   res->it = $step_deserialize(state);
   return res;
}

$WORD $Iterator$map_next($Iterator$map it) {
  $WORD w = it->it->$class->__next__(it->it);
  if (w)
    return it->f(w);
  else
    return NULL;
}

struct $Iterator$map$class $Iterator$map$methods = {"",UNASSIGNED,($Super$class)&$Iterator$methods,$Iterator$map_init,
                                                                $Iterator$map_serialize, $Iterator$map$_deserialize,  
                                                                $Iterator$map_bool,$Iterator$map_str, $Iterator$map_next};

$Iterator$map $Iterator$map$new($Iterator it, $WORD(*f)($WORD)) {
    return $NEW($Iterator$map, it, f);
}

$Iterator $map($Iterable wit, $WORD(*f)($WORD), $WORD iter) {
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
 
$WORD $round($Real wit, $WORD x, $int n) {
  return NULL;
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

$Iterator$zip $Iterator$zip$_deserialize($Serial$state state) {
   $Iterator$zip res = $DNEW($Iterator$zip,state);
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

struct $Iterator$zip$class $Iterator$zip$methods = {"",UNASSIGNED,($Super$class)&$Iterator$methods,$Iterator$zip_init,
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
