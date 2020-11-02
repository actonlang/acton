#include "io_system.h"

struct $Rd_Handler input_handler;
struct $Rd_Handler conn_handler;

void session($Connection conn) {
  $Env env = $NEW($Env,-1); //hmm; we do not have access to the kqueue (and don't need it)...
  input_handler.$handle = (void (*)($WORD, $str))env->$class->stdout_write;
  input_handler.obj = env;
  conn_handler.$handle = (void (*)($WORD, $str))conn->$class->write;
  conn_handler.obj = conn;
  if (conn) {
    conn->$class->on_receipt(conn,&input_handler,&input_handler);
    env->$class->stdin_install(env,&conn_handler);
  } else {
    env->$class->stdout_write(env,to$str("Connection failed"));
    exit(-1);
  }
}
  
int main(int argc, char *argv[]) {
  long port;
  sscanf(argv[2],"%ld",&port);
  int kq = kqueue();
  $Env env = $NEW($Env,kq);
  env->$class->connect(env,to$str(argv[1]),to$int(port),session);
  event_loop(kq);
}
