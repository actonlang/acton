#define GC_THREADS 1
#include "gc.h"

#include <uv.h>
#include "../rts/io.h"
#include "../rts/log.h"
#include "../out/types/file.h"


void fileQ___ext_init__() {

}

$R fileQ_ReadFileD__open_file (fileQ_ReadFile self, $Cont c$cont) {
    pin_actor_affinity();
    uv_fs_t *req = (uv_fs_t *)calloc(1, sizeof(uv_fs_t));
    int r = uv_fs_open(get_uv_loop(), req, (char *)fromB_str(self->filename), UV_FS_O_RDONLY, 0, NULL);
    if (r < 0) {
        char errmsg[1024] = "Error opening file for reading: ";
        uv_strerror_r(r, errmsg + strlen(errmsg), sizeof(errmsg)-strlen(errmsg));
        log_warn(errmsg);
        $RAISE(((B_BaseException)B_RuntimeErrorG_new(to$str(errmsg))));

    }
    self->_fd = to$int(r);
    return $R_CONT(c$cont, B_None);
}


$R fileQ_ReadFileD_closeG_local (fileQ_ReadFile self, $Cont c$cont) {
    uv_fs_t *req = (uv_fs_t *)calloc(1, sizeof(uv_fs_t));
    int r = uv_fs_close(get_uv_loop(), req, (uv_file)from$int(self->_fd), NULL);
    if (r < 0) {
        char errmsg[1024] = "Error closing file: ";
        uv_strerror_r(r, errmsg + strlen(errmsg), sizeof(errmsg)-strlen(errmsg));
        log_warn(errmsg);
        $RAISE(((B_BaseException)B_RuntimeErrorG_new(to$str(errmsg))));
    }
    return $R_CONT(c$cont, B_None);
}

$R fileQ_ReadFileD_readG_local (fileQ_ReadFile self, $Cont c$cont) {
    B_SequenceD_list wit = B_SequenceD_listG_witness;
    uv_fs_t *req = (uv_fs_t *)calloc(1, sizeof(uv_fs_t));
    char buf[1024] = {0};
    uv_buf_t iovec = uv_buf_init(buf, sizeof(buf));
    int r = uv_fs_read(get_uv_loop(), req, (uv_file)from$int(self->_fd), &iovec, 1, -1, NULL);
    B_list res = B_listD_new(0);
    res->length = 0;
    while (r > 0) {
        wit->$class->append(wit, res, to$bytesD_len(buf,r));
        iovec = uv_buf_init(buf, sizeof(buf));
        r = uv_fs_read(get_uv_loop(), req, (uv_file)from$int(self->_fd), &iovec, 1, -1, NULL);
    }
    if (r < 0) {
        char errmsg[1024] = "Error reading from file: ";
        uv_strerror_r(r, errmsg + strlen(errmsg), sizeof(errmsg)-strlen(errmsg));
        log_warn(errmsg);
        $RAISE(((B_BaseException)B_RuntimeErrorG_new(to$str(errmsg))));
    }
    B_bytes nullb = to$bytes("");
    B_Iterable wit2 = ((B_Iterable)((B_Collection)B_SequenceD_listG_new()->W_Collection));
    return $R_CONT(c$cont, nullb->$class->join(nullb,wit2,res));
}


$R fileQ_WriteFileD__open_file (fileQ_WriteFile self, $Cont c$cont) {
    pin_actor_affinity();
    uv_fs_t *req = (uv_fs_t *)calloc(1, sizeof(uv_fs_t));
    int r = uv_fs_open(get_uv_loop(), req, (char *)fromB_str(self->filename),  UV_FS_O_RDWR | UV_FS_O_CREAT, S_IWUSR|S_IRUSR|S_IRGRP|S_IROTH, NULL);
    if (r < 0) {
        char errmsg[1024] = "Error opening file for writing: ";
        uv_strerror_r(r, errmsg + strlen(errmsg), sizeof(errmsg)-strlen(errmsg));
        log_warn(errmsg);
        $RAISE(((B_BaseException)B_RuntimeErrorG_new(to$str(errmsg))));

    }
    self->_fd = to$int(r);
    return $R_CONT(c$cont, B_None);
}

$R fileQ_WriteFileD_closeG_local (fileQ_WriteFile self, $Cont c$cont) {
    uv_fs_t *req = (uv_fs_t *)calloc(1, sizeof(uv_fs_t));
    int r = uv_fs_close(get_uv_loop(), req, (uv_file)from$int(self->_fd), NULL);
    if (r < 0) {
        char errmsg[1024] = "Error closing file: ";
        uv_strerror_r(r, errmsg + strlen(errmsg), sizeof(errmsg)-strlen(errmsg));
        log_warn(errmsg);
        $RAISE(((B_BaseException)B_RuntimeErrorG_new(to$str(errmsg))));
    }
    return $R_CONT(c$cont, B_None);
}

$R fileQ_WriteFileD_writeG_local (fileQ_WriteFile self, $Cont c$cont, B_bytes data) {
    uv_fs_t *req = (uv_fs_t *)calloc(1, sizeof(uv_fs_t));
    uv_buf_t buf = uv_buf_init((char *)data->str, data->nbytes);

    int r = uv_fs_write(get_uv_loop(), req, (uv_file)from$int(self->_fd), &buf, 1, 0, NULL);
    if (r < 0) {
        char errmsg[1024] = "Error writing to file: ";
        uv_strerror_r(r, errmsg + strlen(errmsg), sizeof(errmsg)-strlen(errmsg));
        log_warn(errmsg);
        $RAISE(((B_BaseException)B_RuntimeErrorG_new(to$str(errmsg))));

    }
    return $R_CONT(c$cont, B_None);
}
