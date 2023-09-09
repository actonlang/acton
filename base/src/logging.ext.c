
void loggingQ___ext_init__() {

}

B_int loggingQ_MessageD__get_actor_id (loggingQ_Message self) {
    $Actor actor_self = ($Actor)pthread_getspecific(self_key);
    return to$int(actor_self->$globkey);
}

B_str loggingQ_MessageD__get_actor_class (loggingQ_Message self) {
    $Actor actor_self = ($Actor)pthread_getspecific(self_key);
    return to$str(actor_self->$class->$GCINFO);
}
