void $default__init__($WORD s) {
  return;
}

void $printobj(char *mess,$WORD obj) {
  $struct obj1 = ($struct)obj;
  printf("%s %s\n",mess,obj1->$class->__str__(obj1)->str);
}

