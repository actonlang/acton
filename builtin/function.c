////////////////////////////////////////////////////////////////////////////////////////

void $function$__init__($function $this) { }

$bool $function$__bool__($function self) {
  return $True;
}

$str $function$__str__($function self) {
  char *s;
  asprintf(&s,"<function object at %p>",self);
  return to$str(s);
}

void $function$__serialize__($function self, $Serial$state state) {
    // TBD
}

$function $function$__deserialize__($function self, $Serial$state state) {
    // TBD
    return NULL;
}

struct $function$class $function$methods = {
    "$function",
    UNASSIGNED,
    NULL,
    $function$__init__,
    $function$__serialize__,
    $function$__deserialize__,
    $function$__bool__,
    $function$__str__,
    NULL
};
