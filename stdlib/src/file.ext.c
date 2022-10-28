#include <uv.h>
#include "../rts/io.h"
#include "../rts/log.h"
#include "../out/types/file.h"


void file$$__ext_init__() {

}

$R file$$ReadFile$_open_file (file$$ReadFile __self__, $Cont c$cont) {
    pin_actor_affinity();
    uv_fs_t *req = (uv_fs_t *)calloc(1, sizeof(uv_fs_t));
    int r = uv_fs_open(get_uv_loop(), req, (char *)from$str(__self__->filename), UV_FS_O_RDONLY, 0, NULL);
    if (r < 0) {
        char errmsg[1024] = "Error opening file for reading: ";
        uv_strerror_r(r, errmsg + strlen(errmsg), sizeof(errmsg)-strlen(errmsg));
        log_warn(errmsg);
        $RAISE((($BaseException)$RuntimeError$new(to$str(errmsg))));

    }
    __self__->_fd = to$int(r);
    return $R_CONT(c$cont, $None);
}


$R file$$ReadFile$close$local (file$$ReadFile __self__, $Cont c$cont) {
    uv_fs_t *req = (uv_fs_t *)calloc(1, sizeof(uv_fs_t));
    int r = uv_fs_close(get_uv_loop(), req, (uv_file)from$int(__self__->_fd), NULL);
    if (r < 0) {
        char errmsg[1024] = "Error closing file: ";
        uv_strerror_r(r, errmsg + strlen(errmsg), sizeof(errmsg)-strlen(errmsg));
        log_warn(errmsg);
        $RAISE((($BaseException)$RuntimeError$new(to$str(errmsg))));
    }
    return $R_CONT(c$cont, $None);
}

$R file$$ReadFile$read$local (file$$ReadFile __self__, $Cont c$cont) {
    uv_fs_t *req = (uv_fs_t *)calloc(1, sizeof(uv_fs_t));
    char buf[1024] = {0};
    uv_buf_t iovec = uv_buf_init(buf, sizeof(buf));
    int r = uv_fs_read(get_uv_loop(), req, (uv_file)from$int(__self__->_fd), &iovec, 1, -1, NULL);
    $list res = $list_new(0);
    res->length = 0;
    while (r > 0) {
      $list_append(res, to$bytes_len(buf,r));
        iovec = uv_buf_init(buf, sizeof(buf));
        r = uv_fs_read(get_uv_loop(), req, (uv_file)from$int(__self__->_fd), &iovec, 1, -1, NULL);
    }
    if (r < 0) {
        char errmsg[1024] = "Error reading from file: ";
        uv_strerror_r(r, errmsg + strlen(errmsg), sizeof(errmsg)-strlen(errmsg));
        log_warn(errmsg);
        $RAISE((($BaseException)$RuntimeError$new(to$str(errmsg))));
    }
    $bytes nullb = to$bytes("");
    $Iterable wit = (($Iterable)(($Collection)$Sequence$list$new()->w$Collection));
    return $R_CONT(c$cont, nullb->$class->join(nullb,wit,res));
}


$R file$$WriteFile$_open_file (file$$WriteFile __self__, $Cont c$cont) {
    pin_actor_affinity();
    uv_fs_t *req = (uv_fs_t *)calloc(1, sizeof(uv_fs_t));
    int r = uv_fs_open(get_uv_loop(), req, (char *)from$str(__self__->filename),  UV_FS_O_RDWR | UV_FS_O_CREAT, S_IWUSR|S_IRUSR|S_IRGRP|S_IROTH, NULL);
    if (r < 0) {
        char errmsg[1024] = "Error opening file for writing: ";
        uv_strerror_r(r, errmsg + strlen(errmsg), sizeof(errmsg)-strlen(errmsg));
        log_warn(errmsg);
        $RAISE((($BaseException)$RuntimeError$new(to$str(errmsg))));

    }
    __self__->_fd = to$int(r);
    return $R_CONT(c$cont, $None);
}

$R file$$WriteFile$close$local (file$$WriteFile __self__, $Cont c$cont) {
    uv_fs_t *req = (uv_fs_t *)calloc(1, sizeof(uv_fs_t));
    int r = uv_fs_close(get_uv_loop(), req, (uv_file)from$int(__self__->_fd), NULL);
    if (r < 0) {
        char errmsg[1024] = "Error closing file: ";
        uv_strerror_r(r, errmsg + strlen(errmsg), sizeof(errmsg)-strlen(errmsg));
        log_warn(errmsg);
        $RAISE((($BaseException)$RuntimeError$new(to$str(errmsg))));
    }
    return $R_CONT(c$cont, $None);
}

$R file$$WriteFile$write$local (file$$WriteFile __self__, $Cont c$cont, $bytes data) {
    uv_fs_t *req = (uv_fs_t *)calloc(1, sizeof(uv_fs_t));
    uv_buf_t buf = uv_buf_init((char *)data->str, data->nbytes);

    int r = uv_fs_write(get_uv_loop(), req, (uv_file)from$int(__self__->_fd), &buf, 1, 0, NULL);
    if (r < 0) {
        char errmsg[1024] = "Error writing to file: ";
        uv_strerror_r(r, errmsg + strlen(errmsg), sizeof(errmsg)-strlen(errmsg));
        log_warn(errmsg);
        $RAISE((($BaseException)$RuntimeError$new(to$str(errmsg))));

    }
    return $R_CONT(c$cont, $None);
}
