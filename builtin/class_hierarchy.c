$Serializable $Serializable$new() {
  return $NEW($Serializable);
}

void $Serializable$__init__ ($Serializable self) {
  return;
}

$struct $struct$new() {
  return $NEW($struct);
}

void $struct$__init__ ($struct self) {
  return;
}

$object $object$new() {
  return $NEW($object);
}

void $object$__init__ ($object self) {
  return;
}



struct $Initializable$class $Initializable$methods = {"",UNASSIGNED,NULL,NULL};

struct $Serializable$class $Serializable$methods = {"",UNASSIGNED,($Super$class)&$Initializable$methods, $Serializable$__init__,NULL,NULL};

struct $struct$class $struct$methods = {"",UNASSIGNED,($Super$class)&$Serializable$methods,$struct$__init__,NULL,NULL,NULL,NULL};

struct $object$class $object$methods = {"",UNASSIGNED,($Super$class)&$struct$methods,$object$__init__,NULL,NULL,NULL,NULL};
