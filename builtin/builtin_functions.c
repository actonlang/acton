#include "builtin.h"

// print //////////////////////////////////////////////////////////////////////////////

$WORD mkstr($WORD w) {
  $Initializable w1 = ($Initializable)w;
  return w1->$class->__str__(w);
}

void print($tuple t) {
  $Iterable$opaque iter = $Iterable$pack(($Iterable)$Iterable$tuple$witness,t);
  $str s = $str_join(from$UTF8(""),$Iterable$pack(($Iterable)$Iterable$Iterator$witness,$map(mkstr,iter)));
  printf("%s\n",s->str);
}

// enumerate //////////////////////////////////////////////////////////////////////////

void $Iterator$enumerate_init($Iterator$enumerate self, $Iterator it, $int n) {
  self->it = it;
  self->nxt = n->val;
}

$bool $Iterator$enumerate_bool($Iterator$enumerate self) {
  return $true;
}

$str $Iterator$enumerate_str($Iterator$enumerate self) {
  char *s;
  asprintf(&s,"<enumerate iterator object at %p>",self);
  return from$UTF8(s);
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
  if (w) {
    $WORD comps[] = {w,to$int(it->nxt++)};
    return $NEW($tuple,2,comps);
  } else
    return NULL;
}

struct $Iterator$enumerate$class $Iterator$enumerate$methods = {"",UNASSIGNED,($Super$class)&$Iterator$methods,$Iterator$enumerate_init,
                                                                $Iterator$enumerate_bool,$Iterator$enumerate_str, $Iterator$enumerate_serialize,
                                                                $Iterator$enumerate$_deserialize, $Iterator$enumerate_next};


            
$Iterator $enumerate($Iterable$opaque iter, $int start) {
  $Iterator it = iter->proto->$class->__iter__(iter->proto,iter->impl);
  if (!start)
    start = to$int(0);
  return ($Iterator)$NEW($Iterator$enumerate,it,start); 
}

// filter ////////////////////////////////////////////////////////////////////////////////

void $Iterator$filter_init($Iterator$filter self, $Iterator it, $bool(*f)($WORD)) {
  self->it = it;
  self->f = f;
}

$bool $Iterator$filter_bool($Iterator$filter self) {
  return $true;
}

$str $Iterator$filter_str($Iterator$filter self) {
  char *s;
  asprintf(&s,"<filter iterator object at %p>",self);
  return from$UTF8(s);
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
                                                                $Iterator$filter_bool,$Iterator$filter_str, $Iterator$filter_serialize,
                                                                $Iterator$filter$_deserialize, $Iterator$filter_next};

$Iterator $filter($bool(*f)($WORD),$Iterable$opaque iter) {
  $Iterator it = iter->proto->$class->__iter__(iter->proto,iter->impl);
  return ($Iterator)$NEW($Iterator$filter,it,f);
}

// map ////////////////////////////////////////////////////////////////////////////////

void $Iterator$map_init($Iterator$map self, $Iterator it, $WORD(*f)($WORD)) {
  self->it = it;
  self->f = f;
}

$bool $Iterator$map_bool($Iterator$map self) {
  return $true;
}

$str $Iterator$map_str($Iterator$map self) {
  char *s;
  asprintf(&s,"<map iterator object at %p>",self);
  return from$UTF8(s);
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
                                                                $Iterator$map_bool,$Iterator$map_str, $Iterator$map_serialize,
                                                                $Iterator$map$_deserialize, $Iterator$map_next};

$Iterator $map($WORD(*f)($WORD),$Iterable$opaque iter) {
  $Iterator it = iter->proto->$class->__iter__(iter->proto,iter->impl);
  return ($Iterator)$NEW($Iterator$map,it,f);
}


$WORD $max($Iterable$opaque iter,$WORD deflt) {
  return NULL;
}

$WORD $min($Iterable$opaque iter,$WORD deflt) {
  return NULL;
}
 
// print

$Real$opaque $round($Real$opaque x, $int n) {
  return NULL;
}

$list $sorted($Iterable$opaque iter) {
  return NULL;
}

$WORD $sum($Iterable$opaque iter, $WORD start) {
  return NULL;
}

$Iterator $zip ($Iterable$opaque iter1, $Iterable$opaque iter2) {
  return NULL;
}
