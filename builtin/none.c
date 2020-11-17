$NoneType $NoneType$new() {
  return $NEW($NoneType);
}


void $NoneType__serialize__($NoneType self, $Serial$state state) {
  $add_header(NONE_ID,0,state);
}

$NoneType $NoneType__deserialize__( $Serial$state state) {
  state->row = state->row->next;
  state->row_no++;
  return NULL;
}

$bool $NoneType__bool__($NoneType self) {
  return $False;
}

$str $NoneType__str__($NoneType self) {
  return to$str("None");
}

struct $NoneType$class $NoneType$methods = {"",UNASSIGNED,($Super$class)&$struct$methods,(void (*)($NoneType))$default__init__,
                                            $NoneType__serialize__,  $NoneType__deserialize__, $NoneType__bool__, $NoneType__str__};
