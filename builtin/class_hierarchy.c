$Serializable $Serializable$new() {
  return $NEW($Serializable);
}

void $Serializable$__init__ ($Serializable self) {
  return;
}

$value $value$new() {
  return $NEW($value);
}

void $value$__init__ ($value self) {
  return;
}

$object $object$new() {
  return $NEW($object);
}

void $object$__init__ ($object self) {
  return;
}



struct $Initializable$class $Initializable$methods = {"$Initializable",UNASSIGNED,NULL,NULL};

struct $Serializable$class $Serializable$methods = {"$Serializable",UNASSIGNED,($Super$class)&$Initializable$methods, $Serializable$__init__,NULL,NULL};

struct $value$class $value$methods = {"$value",UNASSIGNED,($Super$class)&$Serializable$methods,$value$__init__,NULL,NULL,NULL,NULL};

struct $object$class $object$methods = {"$value",UNASSIGNED,($Super$class)&$value$methods,$object$__init__,NULL,NULL,NULL,NULL};
