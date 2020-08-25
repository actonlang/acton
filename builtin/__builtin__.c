
$Eq$opaque $Eq$pack($Eq proto, $WORD impl) {
  $Eq$opaque res = malloc(sizeof(struct $Eq$opaque));
  res->proto = proto;
  res->impl = impl;
  return res;
}

$Ord$opaque $Ord$pack($Ord proto, $WORD impl){
  $Ord$opaque res = malloc(sizeof(struct $Ord$opaque));
  res->proto = proto;
  res->impl = impl;
  return res;
}

$Logical$opaque $Logical$pack($Logical proto, $WORD impl){
  $Logical$opaque res = malloc(sizeof(struct $Logical$opaque));
  res->proto = proto;
  res->impl = impl;
  return res;
}

$Plus$opaque $Plus$pack($Plus proto, $WORD impl){
  $Plus$opaque res = malloc(sizeof(struct $Plus$opaque));
  res->proto = proto;
  res->impl = impl;
  return res;
}

$Minus$opaque $Minus$pack($Minus proto, $WORD impl){
  $Minus$opaque res = malloc(sizeof(struct $Minus$opaque));
  res->proto = proto;
  res->impl = impl;
  return res;
}

$Hashable$opaque $Hashable$pack($Hashable proto, $WORD impl){
  $Hashable$opaque res = malloc(sizeof(struct $Hashable$opaque));
  res->proto = proto;
  res->impl = impl;
  return res;
}

$Iterable$opaque $Iterable$pack($Iterable proto, $WORD impl){
  $Iterable$opaque res = malloc(sizeof(struct $Iterable$opaque));
  res->proto = proto;
  res->impl = impl;
  return res;
}

$Collection$opaque $Collection$pack($Collection proto, $WORD impl){
  $Collection$opaque res = malloc(sizeof(struct $Collection$opaque));
  res->proto = proto;
  res->impl = impl;
  return res;
}

$Indexed$opaque $Indexed$pack($Indexed proto, $WORD impl){
  $Indexed$opaque res = malloc(sizeof(struct $Indexed$opaque));
  res->proto = proto;
  res->impl = impl;
  return res;
}

$Sliceable$opaque $Sliceable$pack($Sliceable proto, $WORD impl){
  $Sliceable$opaque res = malloc(sizeof(struct $Sliceable$opaque));
  res->proto = proto;
  res->impl = impl;
  return res;
}

$Sequence$opaque $Sequence$pack($Sequence proto, $WORD impl){
  $Sequence$opaque res = malloc(sizeof(struct $Sequence$opaque));
  res->proto = proto;
  res->impl = impl;
  return res;
}

$Container$opaque $Container$pack($Container proto, $WORD impl){
  $Container$opaque res = malloc(sizeof(struct $Container$opaque));
  res->proto = proto;
  res->impl = impl;
  return res;
}

$Mapping$opaque $Mapping$pack($Mapping proto, $WORD impl){
  $Mapping$opaque res = malloc(sizeof(struct $Mapping$opaque));
  res->proto = proto;
  res->impl = impl;
  return res;
}

$Set$opaque $Set$pack($Set proto, $WORD impl){
  $Set$opaque res = malloc(sizeof(struct $Set$opaque));
  res->proto = proto;
  res->impl = impl;
  return res;
}

$Number$opaque $Number$pack($Number proto, $WORD impl){
  $Number$opaque res = malloc(sizeof(struct $Number$opaque));
  res->proto = proto;
  res->impl = impl;
  return res;
}

$Real$opaque $Real$pack($Real proto, $WORD impl){
  $Real$opaque res = malloc(sizeof(struct $Real$opaque));
  res->proto = proto;
  res->impl = impl;
  return res;
}

$Rational$opaque $Rational$pack($Rational proto, $WORD impl){
  $Rational$opaque res = malloc(sizeof(struct $Rational$opaque));
  res->proto = proto;
  res->impl = impl;
  return res;
}

$Integral$opaque $Integral$pack($Integral proto, $WORD impl){
  $Integral$opaque res = malloc(sizeof(struct $Integral$opaque));
  res->proto = proto;
  res->impl = impl;
  return res;
}
