struct $bytearray$class;

struct $bytearray {
  struct $bytearray$class *$class;
  int nbytes;        
  unsigned char *str;
};

struct $bytearray$class {
  char *$GCINFO;
  int $class_id;
  $Super$class $superclass;
  void (*__init__)($bytearray, $Sequence$opaque);
  void (*__serialize__)($bytearray,$Serial$state);
  $bytearray (*__deserialize__)($Serial$state);
  $bool (*__bool__)($bytearray);
  $str (*__str__)($bytearray);
  $bytearray (*capitalize)($bytearray s);
  $bytearray (*center)($bytearray s, $int width, $bytearray fill);                 
  $int (*count)($bytearray s, $bytearray sub, $int start, $int end);
  $str (*decode)($bytearray);
  $bool (*endswith)($bytearray s, $bytearray suffix, $int start, $int end);
  $bytearray (*expandtabs)($bytearray s, $int tabsize);     
  $int (*find)($bytearray s, $bytearray sub, $int start, $int end);         
  $int (*index)($bytearray s, $bytearray sub, $int start, $int end);        
  $bool (*isalnum)($bytearray s);                                     
  $bool (*isalpha)($bytearray s);
  $bool (*isascii)($bytearray s);
  $bool (*isdigit)($bytearray s);
  $bool (*islower)($bytearray s);
  //  $bool (*isprintable)($bytearray s);
  $bool (*isspace)($bytearray s);
  $bool (*istitle)($bytearray s);
  $bool (*isupper)($bytearray s);
  $bytearray (*join)($bytearray sep, $Iterable$opaque it);
  $bytearray (*ljust)($bytearray s, $int width, $bytearray fill);                  
  $bytearray (*lower)($bytearray s);
  $bytearray (*lstrip)($bytearray s,$bytearray cs);                               
  $tuple (*partition)($bytearray s, $bytearray sep);
  $bytearray (*replace)($bytearray s, $bytearray old, $bytearray new, $int count);
  $int (*rfind)($bytearray s, $bytearray sub, $int start, $int end);
  $int (*rindex)($bytearray s, $bytearray sub, $int start, $int end);       
  $bytearray (*rjust)($bytearray s, $int width, $bytearray fill);                  
  $tuple (*rpartition)($bytearray s, $bytearray sep); 
  //$list (*rsplit)($bytearray s, $bytearray sep, int maxsplit);             
  $bytearray (*rstrip)($bytearray s,$bytearray cs);                                
  $list (*split)($bytearray s, $bytearray sep, $int maxsplit);               
  $list (*splitlines)($bytearray s, $bool keepends);                                   
  $bool (*startswith)($bytearray s, $bytearray prefix, $int start, $int end);
  $bytearray (*strip)($bytearray s, $bytearray cs);                                
  $bytearray (*upper)($bytearray s);
  $bytearray (*zfill)($bytearray s, $int width);
};

extern struct $bytearray$class $bytearray$methods;

extern struct $Ord$bytearray$class $Ord$bytearray$methods;
extern struct $Hashable$bytearray$class $Hashable$bytearray$methods;
extern struct $Plus$bytearray$class $Plus$bytearraymethods;
extern struct $Sliceable$bytearray$class $Sliceable$bytearray$methods;
extern struct $Container$bytearray$class $Container$bytearray$methods;

extern struct $Ord$bytearray *$Ord$bytearray$witness;
extern struct $Hashable$bytearray *$Hashable$bytearray$witness;
extern struct $Plus$bytearray *$Plus$bytearray$witness;
extern struct $Sliceable$bytearray *$Sliceable$bytearray$witness;
extern struct $Container$bytearray *$Container$bytearray$witness;
