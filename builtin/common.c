void $default__init__($WORD s) {
  return;
}

void $printobj(char *mess,$WORD obj) {
  $value obj1 = ($value)obj;
  printf("%s %s\n",mess,obj1->$class->__str__(obj1)->str);
}

