struct $atom$class {
  char *$GCINFO;
  int $class_id;
  $Super$class $superclass;
  void (*__init__)($atom, $WORD);
  void (*__serialize__)($atom,$Serial$state);
  $atom (*__deserialize__)($atom,$Serial$state);
  $bool (*__bool__)($atom);
  $str (*__str__)($atom);
  $str (*__repr__)($atom);
};

struct $atom {
  struct $atom$class *$class;
};

extern struct $atom$class $atom$methods;


