#define GC_THREADS 1
#include "gc.h"

#include <sys/file.h>

#include <uv.h>
#include "../rts/io.h"
#include "../rts/log.h"
#include "../out/types/file.h"


void fileQ___ext_init__() {

}

$R fileQ_FSD__pin_affinityG_local (fileQ_FS self, $Cont c$cont) {
    pin_actor_affinity();
    return $R_CONT(c$cont, B_None);
}

// def is_dir(self) -> bool:
B_bool fileQ_FileStatD_is_dir (fileQ_FileStat self) {
    return toB_bool(S_ISDIR(fromB_u64(self->mode)));
}

// def is_file(self) -> bool:
B_bool fileQ_FileStatD_is_file (fileQ_FileStat self) {
    return toB_bool(S_ISREG(fromB_u64(self->mode)));
}

// def is_symlink(self) -> bool:
B_bool fileQ_FileStatD_is_symlink (fileQ_FileStat self) {
#if defined(_WIN32) || defined(_WIN64)
    // TODO: do better
    return B_False;
#else
    return toB_bool(S_ISLNK(fromB_u64(self->mode)));
#endif
}

// def is_block_device(self) -> bool:
B_bool fileQ_FileStatD_is_block_device (fileQ_FileStat self) {
    return toB_bool(S_ISBLK(fromB_u64(self->mode)));
}

// def is_char_device(self) -> bool:
B_bool fileQ_FileStatD_is_char_device (fileQ_FileStat self) {
    return toB_bool(S_ISCHR(fromB_u64(self->mode)));
}

// def is_fifo(self) -> bool:
B_bool fileQ_FileStatD_is_fifo (fileQ_FileStat self) {
#if defined(_WIN32) || defined(_WIN64)
    // TODO: do better
    return B_False;
#else
    return toB_bool(S_ISFIFO(fromB_u64(self->mode)));
#endif
}

// def is_socket(self) -> bool:
B_bool fileQ_FileStatD_is_socket (fileQ_FileStat self) {
#if defined(_WIN32) || defined(_WIN64)
    // TODO: do better
    return B_False;
#else
    return toB_bool(S_ISSOCK(fromB_u64(self->mode)));
#endif
}

// action def copyfile(src: str, dst: str) -> None:
$R fileQ_FSD_copyfileG_local (fileQ_FS self, $Cont C_cont, B_str src, B_str dst) {
    uv_fs_t *req = (uv_fs_t *)acton_malloc(sizeof(uv_fs_t));
    int r = uv_fs_copyfile(get_uv_loop(), req, (char *)fromB_str(src), (char *)fromB_str(dst), 0, NULL);
    if (r < 0) {
        char errmsg[1024] = "Error copying file: ";
        uv_strerror_r(r, errmsg + strlen(errmsg), sizeof(errmsg)-strlen(errmsg));
        log_warn(errmsg);
        $RAISE(((B_BaseException)B_OSErrorG_new(to$str(errmsg))));
    }
    return $R_CONT(C_cont, B_None);
}

// action def cwd() -> str:
$R fileQ_FSD_cwdG_local (fileQ_FS self, $Cont C_cont) {
    char cwd[1024];
    size_t size = sizeof(cwd);
    int r = uv_cwd(cwd, &size);
    if (r < 0) {
        char errmsg[1024] = "Error getting cwd: ";
        uv_strerror_r(r, errmsg + strlen(errmsg), sizeof(errmsg)-strlen(errmsg));
        log_warn(errmsg);
        $RAISE(((B_BaseException)B_OSErrorG_new(to$str(errmsg))));
    }
    return $R_CONT(C_cont, to$str(cwd));
}

// action def exepath() -> str:
$R fileQ_FSD_exepathG_local (fileQ_FS self, $Cont C_cont) {
    char exepath[1024];
    size_t size = sizeof(exepath);
    int r = uv_exepath(exepath, &size);
    if (r < 0) {
        char errmsg[1024] = "Error getting exepath: ";
        uv_strerror_r(r, errmsg + strlen(errmsg), sizeof(errmsg)-strlen(errmsg));
        log_warn(errmsg);
        $RAISE(((B_BaseException)B_OSErrorG_new(to$str(errmsg))));
    }
    return $R_CONT(C_cont, to$str(exepath));
}

// action def homedir() -> str:
$R fileQ_FSD_homedirG_local (fileQ_FS self, $Cont C_cont) {
    char homedir[1024];
    size_t size = sizeof(homedir);
    int r = uv_os_homedir(homedir, &size);
    if (r < 0) {
        char errmsg[1024] = "Error getting homedir: ";
        uv_strerror_r(r, errmsg + strlen(errmsg), sizeof(errmsg)-strlen(errmsg));
        log_warn(errmsg);
        $RAISE(((B_BaseException)B_OSErrorG_new(to$str(errmsg))));
    }
    return $R_CONT(C_cont, to$str(homedir));
}

// action def mkdir(filename: str):
$R fileQ_FSD_mkdirG_local (fileQ_FS self, $Cont C_cont, B_str filename) {
    uv_fs_t *req = (uv_fs_t *)acton_malloc(sizeof(uv_fs_t));
    int r = uv_fs_mkdir(get_uv_loop(), req, (char *)fromB_str(filename), 0777, NULL);
    if (r < 0) {
        char errmsg[1024] = "Error creating directory: ";
        uv_strerror_r(r, errmsg + strlen(errmsg), sizeof(errmsg)-strlen(errmsg));
        log_warn(errmsg);
        $RAISE(((B_BaseException)B_OSErrorG_new(to$str(errmsg))));
    }
    return $R_CONT(C_cont, B_None);
}

// action def listdir(path: str) -> list[str]:
$R fileQ_FSD_listdirG_local (fileQ_FS self, $Cont C_cont, B_str path) {
    B_SequenceD_list wit = B_SequenceD_listG_witness;
    uv_fs_t *req = (uv_fs_t *)acton_malloc(sizeof(uv_fs_t));
    B_list res = B_listD_new(0);
    res->length = 0;
    int r = uv_fs_scandir(get_uv_loop(), req, (char *)fromB_str(path), 0, NULL);
    if (r < 0) {
        char errmsg[1024] = "Error listing directory: ";
        uv_strerror_r(r, errmsg + strlen(errmsg), sizeof(errmsg)-strlen(errmsg));
        log_warn(errmsg);
        $RAISE(((B_BaseException)B_OSErrorG_new(to$str(errmsg))));
    }
    uv_dirent_t ent;
    while (uv_fs_scandir_next(req, &ent) != UV_EOF) {
        wit->$class->append(wit, res, to$str(ent.name));
    }
    return $R_CONT(C_cont, res);
}

// action def lstat(filename: str) -> FileStat:
$R fileQ_FSD_lstatG_local (fileQ_FS self, $Cont C_cont, B_str filename) {
    uv_fs_t *req = (uv_fs_t *)acton_malloc(sizeof(uv_fs_t));
    int r = uv_fs_lstat(get_uv_loop(), req, (char *)fromB_str(filename), NULL);
    if (r < 0) {
        char errmsg[1024] = "Error getting file stat: ";
        uv_strerror_r(r, errmsg + strlen(errmsg), sizeof(errmsg)-strlen(errmsg));
        log_warn(errmsg);
        $RAISE((B_BaseException)B_OSErrorG_new(to$str(errmsg)));
    }
    uv_stat_t *stat = (uv_stat_t *)req->ptr;
    fileQ_FileStat res = fileQ_FileStatG_new(filename,
                                             toB_u64(stat->st_dev),
                                             toB_u64(stat->st_mode),
                                             toB_u64(stat->st_nlink),
                                             toB_u64(stat->st_uid),
                                             toB_u64(stat->st_gid),
                                             toB_u64(stat->st_rdev),
                                             toB_u64(stat->st_ino),
                                             toB_u64(stat->st_size),
                                             toB_u64(stat->st_blksize),
                                             toB_u64(stat->st_blocks),
                                             toB_u64(stat->st_flags),
                                             toB_u64(stat->st_gen),
                                             toB_float(stat->st_atim.tv_sec + stat->st_atim.tv_nsec / 1e9),
                                             toB_float(stat->st_mtim.tv_sec + stat->st_mtim.tv_nsec / 1e9),
                                             toB_float(stat->st_ctim.tv_sec + stat->st_ctim.tv_nsec / 1e9),
                                             toB_float(stat->st_birthtim.tv_sec + stat->st_birthtim.tv_nsec / 1e9)
                                             );
    return $R_CONT(C_cont, res);
}

// action def rmdir(dirname: str) -> None:
$R fileQ_FSD_rmdirG_local (fileQ_FS self, $Cont C_cont, B_str dirname) {
    uv_fs_t *req = (uv_fs_t *)acton_malloc(sizeof(uv_fs_t));
    int r = uv_fs_rmdir(get_uv_loop(), req, (char *)fromB_str(dirname), NULL);
    if (r < 0) {
        char errmsg[1024] = "Error removing directory: ";
        uv_strerror_r(r, errmsg + strlen(errmsg), sizeof(errmsg)-strlen(errmsg));
        log_warn(errmsg);
        $RAISE(((B_BaseException)B_OSErrorG_new(to$str(errmsg))));
    }
    return $R_CONT(C_cont, B_None);
}

// action def remove(filename: str) -> None:
$R fileQ_FSD_removeG_local (fileQ_FS self, $Cont C_cont, B_str filename) {
    uv_fs_t *req = (uv_fs_t *)acton_malloc(sizeof(uv_fs_t));
    int r = uv_fs_unlink(get_uv_loop(), req, (char *)fromB_str(filename), NULL);
    if (r < 0) {
        char errmsg[1024] = "Error removing file: ";
        uv_strerror_r(r, errmsg + strlen(errmsg), sizeof(errmsg)-strlen(errmsg));
        log_warn(errmsg);
        $RAISE(((B_BaseException)B_OSErrorG_new(to$str(errmsg))));
    }
    return $R_CONT(C_cont, B_None);
}

// action def stat(filename: str) -> FileStat:
$R fileQ_FSD_statG_local (fileQ_FS self, $Cont C_cont, B_str filename) {
    uv_fs_t *req = (uv_fs_t *)acton_malloc(sizeof(uv_fs_t));
    int r = uv_fs_stat(get_uv_loop(), req, (char *)fromB_str(filename), NULL);
    if (r == UV_ENOENT) {
        $RAISE(((B_BaseException)B_FileNotFoundErrorG_new(filename)));
    } else if (r < 0) {
        char errmsg[1024] = "Error getting file stat: ";
        uv_strerror_r(r, errmsg + strlen(errmsg), sizeof(errmsg)-strlen(errmsg));
        log_warn(errmsg);
        $RAISE(((B_BaseException)B_OSErrorG_new(to$str(errmsg))));
    }
    uv_stat_t *stat = (uv_stat_t *)req->ptr;
    fileQ_FileStat res = fileQ_FileStatG_new(filename,
                                             toB_u64(stat->st_dev),
                                             toB_u64(stat->st_mode),
                                             toB_u64(stat->st_nlink),
                                             toB_u64(stat->st_uid),
                                             toB_u64(stat->st_gid),
                                             toB_u64(stat->st_rdev),
                                             toB_u64(stat->st_ino),
                                             toB_u64(stat->st_size),
                                             toB_u64(stat->st_blksize),
                                             toB_u64(stat->st_blocks),
                                             toB_u64(stat->st_flags),
                                             toB_u64(stat->st_gen),
                                             toB_float(stat->st_atim.tv_sec + stat->st_atim.tv_nsec / 1e9),
                                             toB_float(stat->st_mtim.tv_sec + stat->st_mtim.tv_nsec / 1e9),
                                             toB_float(stat->st_ctim.tv_sec + stat->st_ctim.tv_nsec / 1e9),
                                             toB_float(stat->st_birthtim.tv_sec + stat->st_birthtim.tv_nsec / 1e9)
                                             );
    return $R_CONT(C_cont, res);
}

$R fileQ_FSD_tmpdirG_local (fileQ_FS self, $Cont C_cont) {
    size_t size = 1024; // Initial buffer size for the tmp directory path
    char *buffer = (char*)acton_malloc(size);
    int r = uv_os_tmpdir(buffer, &size);
    if (r < 0) {
        char errmsg[1024] = "Error getting temporary directory: ";
        uv_strerror_r(r, errmsg + strlen(errmsg), sizeof(errmsg)-strlen(errmsg));
        log_warn(errmsg);
        $RAISE(((B_BaseException)B_OSErrorG_new(to$str(errmsg))));
    }
    return $R_CONT(C_cont, to$str(buffer));
}

$R fileQ_ReadFileD__open_fileG_local (fileQ_ReadFile self, $Cont c$cont) {
    pin_actor_affinity();
    uv_fs_t *req = (uv_fs_t *)acton_malloc(sizeof(uv_fs_t));
    int r = uv_fs_open(get_uv_loop(), req, (char *)fromB_str(self->filename), UV_FS_O_RDONLY, 0, NULL);
    if (r == UV_ENOENT) {
        $RAISE(((B_BaseException)B_FileNotFoundErrorG_new(self->filename)));
    } else if (r < 0) {
        char errmsg[1024] = "Error opening file for reading: ";
        uv_strerror_r(r, errmsg + strlen(errmsg), sizeof(errmsg)-strlen(errmsg));
        log_warn(errmsg);
        $RAISE(((B_BaseException)B_OSErrorG_new(to$str(errmsg))));

    }
    self->_fd = to$int(r);
    return $R_CONT(c$cont, B_None);
}

$R fileQ_ReadFileD__lock_fileG_local (fileQ_ReadFile self, $Cont c$cont) {
#if defined(_WIN32) || defined(_WIN64)
    assert(0 && "fileQ_ReadFileD__lock_fileG_local not implemented on Windows");
#else
    int r = flock(from$int(self->_fd), LOCK_EX + LOCK_NB);
    if (r < 0) {
        char errmsg[1024] = "Error locking file: ";
        uv_strerror_r(r, errmsg + strlen(errmsg), sizeof(errmsg)-strlen(errmsg));
        log_warn(errmsg);
        $RAISE(((B_BaseException)B_OSErrorG_new(to$str(errmsg))));
    }
#endif
    return $R_CONT(c$cont, B_None);
}

$R fileQ_ReadFileD_closeG_local (fileQ_ReadFile self, $Cont c$cont) {
    uv_fs_t *req = (uv_fs_t *)acton_malloc(sizeof(uv_fs_t));
    int r = uv_fs_close(get_uv_loop(), req, (uv_file)from$int(self->_fd), NULL);
    if (r < 0) {
        char errmsg[1024] = "Error closing file: ";
        uv_strerror_r(r, errmsg + strlen(errmsg), sizeof(errmsg)-strlen(errmsg));
        log_warn(errmsg);
        $RAISE(((B_BaseException)B_OSErrorG_new(to$str(errmsg))));
    }
    return $R_CONT(c$cont, B_None);
}

$R fileQ_ReadFileD_readG_local (fileQ_ReadFile self, $Cont c$cont) {
    B_SequenceD_list wit = B_SequenceD_listG_witness;
    uv_fs_t *req = (uv_fs_t *)acton_malloc(sizeof(uv_fs_t));
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
        $RAISE(((B_BaseException)B_OSErrorG_new(to$str(errmsg))));
    }
    B_bytes nullb = to$bytes("");
    B_Iterable wit2 = ((B_Iterable)((B_Collection)B_SequenceD_listG_new()->W_Collection));
    return $R_CONT(c$cont, nullb->$class->join(nullb,wit2,res));
}


$R fileQ_WriteFileD__open_fileG_local (fileQ_WriteFile self, $Cont c$cont) {
    pin_actor_affinity();
    uv_fs_t *req = (uv_fs_t *)acton_malloc(sizeof(uv_fs_t));
    int r = uv_fs_open(get_uv_loop(), req, (char *)fromB_str(self->filename),  UV_FS_O_RDWR | UV_FS_O_CREAT | UV_FS_O_TRUNC, S_IWUSR|S_IRUSR|S_IRGRP|S_IROTH, NULL);
    if (r < 0) {
        char errmsg[1024] = "Error opening file for writing: ";
        uv_strerror_r(r, errmsg + strlen(errmsg), sizeof(errmsg)-strlen(errmsg));
        log_warn(errmsg);
        $RAISE(((B_BaseException)B_OSErrorG_new(to$str(errmsg))));

    }
    self->_fd = to$int(r);
    return $R_CONT(c$cont, B_None);
}

$R fileQ_WriteFileD__lock_fileG_local (fileQ_WriteFile self, $Cont c$cont) {
#if defined(_WIN32) || defined(_WIN64)
    assert(0 && "fileQ_ReadFileD__lock_fileG_local not implemented on Windows");
#else
    int r = flock(from$int(self->_fd), LOCK_EX + LOCK_NB);
    if (r < 0) {
        char errmsg[1024] = "Error locking file: ";
        uv_strerror_r(r, errmsg + strlen(errmsg), sizeof(errmsg)-strlen(errmsg));
        log_warn(errmsg);
        $RAISE(((B_BaseException)B_OSErrorG_new(to$str(errmsg))));
    }
#endif
    return $R_CONT(c$cont, B_None);
}


$R fileQ_WriteFileD_closeG_local (fileQ_WriteFile self, $Cont c$cont) {
    uv_fs_t *req = (uv_fs_t *)acton_malloc(sizeof(uv_fs_t));
    int r = uv_fs_close(get_uv_loop(), req, (uv_file)from$int(self->_fd), NULL);
    if (r < 0) {
        char errmsg[1024] = "Error closing file: ";
        uv_strerror_r(r, errmsg + strlen(errmsg), sizeof(errmsg)-strlen(errmsg));
        log_warn(errmsg);
        $RAISE(((B_BaseException)B_OSErrorG_new(to$str(errmsg))));
    }
    return $R_CONT(c$cont, B_None);
}

$R fileQ_WriteFileD_writeG_local (fileQ_WriteFile self, $Cont c$cont, B_bytes data) {
    uv_fs_t *req = (uv_fs_t *)acton_malloc(sizeof(uv_fs_t));
    uv_buf_t buf = uv_buf_init((char *)data->str, data->nbytes);

    int r = uv_fs_write(get_uv_loop(), req, (uv_file)from$int(self->_fd), &buf, 1, 0, NULL);
    if (r < 0) {
        char errmsg[1024] = "Error writing to file: ";
        uv_strerror_r(r, errmsg + strlen(errmsg), sizeof(errmsg)-strlen(errmsg));
        log_warn(errmsg);
        $RAISE(((B_BaseException)B_OSErrorG_new(to$str(errmsg))));

    }
    return $R_CONT(c$cont, B_None);
}
