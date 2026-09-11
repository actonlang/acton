/* Live observations for the performance scaling controller, in bytes.
 * These are sampled estimates, not a reservation or an OOM guarantee. */
#include <stdint.h>
#include <stddef.h>
#include <stdio.h>
#include <stdarg.h>
#include <errno.h>
#include <string.h>

static int memory_error(char *error, size_t size, const char *format, ...) {
    va_list args;
    va_start(args, format);
    vsnprintf(error, size, format, args);
    va_end(args);
    return -1;
}

static uint64_t smaller(uint64_t a, uint64_t b) {
    return a < b ? a : b;
}

#if defined(__linux__)
#include <stdlib.h>
#include <inttypes.h>
#include <limits.h>
#include <sys/stat.h>
#include <unistd.h>

static int has_word(const char *list, const char *word) {
    size_t n = strlen(word);
    for (const char *p = list; p && *p; p = strchr(p, ',')) {
        if (*p == ',') p++;
        if (strncmp(p, word, n) == 0 && (p[n] == ',' || p[n] == '\0')) return 1;
    }
    return 0;
}

/* mountinfo escapes whitespace and backslashes as octal sequences. */
static void unescape_path(char *path) {
    char *out = path;
    for (char *p = path; *p; p++) {
        if (*p == '\\' && p[1] >= '0' && p[1] <= '7'
            && p[2] >= '0' && p[2] <= '7' && p[3] >= '0' && p[3] <= '7') {
            *out++ = (char)((p[1] - '0') * 64 + (p[2] - '0') * 8 + p[3] - '0');
            p += 3;
        } else {
            *out++ = *p;
        }
    }
    *out = '\0';
}

static int cgroup_path(int pid, int *version, char *group, char *error, size_t size) {
    char path[64];
    snprintf(path, sizeof(path), "/proc/%d/cgroup", pid);
    FILE *file = fopen(path, "r");
    if (!file) return memory_error(error, size, "Cannot read %s: %s", path, strerror(errno));
    char *line = NULL;
    size_t capacity = 0;
    *version = 0;
    while (getline(&line, &capacity, file) >= 0) {
        char *controllers = strchr(line, ':');
        char *member = controllers ? strchr(controllers + 1, ':') : NULL;
        if (!member) continue;
        *controllers++ = '\0';
        *member++ = '\0';
        member[strcspn(member, "\n")] = '\0';
        int candidate = has_word(controllers, "memory") ? 1 : strcmp(line, "0") == 0 ? 2 : 0;
        if (!candidate || (*version == 1 && candidate == 2)) continue;
        if (member[0] != '/' || strlen(member) >= PATH_MAX
            || strstr(member, "/../") || strcmp(member, "/..") == 0
            || (strlen(member) >= 3 && strcmp(member + strlen(member) - 3, "/..") == 0)) {
            free(line);
            fclose(file);
            return memory_error(error, size, "Cannot resolve process memory cgroup");
        }
        strcpy(group, member);
        *version = candidate;
    }
    int failed = ferror(file);
    free(line);
    fclose(file);
    return failed ? memory_error(error, size, "Cannot read %s", path) : 0;
}

static int cgroup_mount(int version, const char *group, char *mount, char *directory,
                        char *error, size_t size) {
    FILE *file = fopen("/proc/self/mountinfo", "r");
    if (!file) return memory_error(error, size, "Cannot read cgroup mounts: %s", strerror(errno));
    char *line = NULL;
    size_t capacity = 0, best = SIZE_MAX;
    while (getline(&line, &capacity, file) >= 0) {
        char *separator = strstr(line, " - ");
        if (!separator) continue;
        char *tail = separator + 3, *save = NULL;
        char *type = strtok_r(tail, " ", &save);
        char *source = strtok_r(NULL, " ", &save);
        char *options = strtok_r(NULL, " \n", &save);
        if (!type || !source || !options) continue;
        if (version == 2 ? strcmp(type, "cgroup2") != 0
                         : strcmp(type, "cgroup") != 0 || !has_word(options, "memory")) continue;
        *separator = '\0';
        save = NULL;
        char *root = NULL, *point = NULL;
        for (int field = 1; field <= 5; field++) {
            char *value = strtok_r(field == 1 ? line : NULL, " ", &save);
            if (!value) break;
            if (field == 4) root = value;
            if (field == 5) point = value;
        }
        if (!root || !point) continue;
        unescape_path(root);
        unescape_path(point);
        size_t n = strlen(root);
        const char *relative = NULL;
        if (strcmp(root, "/") == 0) relative = group;
        else if (strncmp(group, root, n) == 0 && (group[n] == '/' || group[n] == '\0')) relative = group + n;
        /* Prefer the mount exposing the most ancestors. A namespace can still
         * hide limits above its root; they cannot be observed from here. */
        if (!relative || n >= best || strlen(point) + strlen(relative) >= PATH_MAX) continue;
        strcpy(mount, point);
        snprintf(directory, PATH_MAX, "%s%s", point, relative);
        size_t length = strlen(directory);
        if (length > 1 && directory[length - 1] == '/') directory[length - 1] = '\0';
        best = n;
    }
    int failed = ferror(file);
    free(line);
    fclose(file);
    if (failed || best == SIZE_MAX) return memory_error(error, size, "Cannot resolve process memory cgroup mount");
    return 0;
}

/* 0 means absent (a root or disabled controller), 1 a value, -1 an error. */
static int cgroup_number(const char *directory, const char *name, uint64_t *value,
                         char *error, size_t size) {
    char path[PATH_MAX + 64];
    snprintf(path, sizeof(path), "%s/%s", directory, name);
    FILE *file = fopen(path, "r");
    if (!file) {
        if (errno == ENOENT) return 0;
        return memory_error(error, size, "Cannot read %s: %s", path, strerror(errno));
    }
    char text[80], extra;
    int count = fscanf(file, "%79s %c", text, &extra);
    fclose(file);
    if (count != 1) return memory_error(error, size, "Invalid memory counter in %s", path);
    if (strcmp(text, "max") == 0 && strcmp(name, "memory.max") == 0) {
        *value = UINT64_MAX;
        return 1;
    }
    char *end;
    errno = 0;
    unsigned long long parsed = strtoull(text, &end, 10);
    if (text[0] < '0' || text[0] > '9' || *end || errno == ERANGE)
        return memory_error(error, size, "Invalid memory counter in %s", path);
    *value = (uint64_t)parsed;
    return 1;
}

static int cgroup_memory(int pid, uint64_t *total, uint64_t *available, char *error, size_t size) {
    int version;
    char group[PATH_MAX], mount[PATH_MAX], directory[PATH_MAX];
    if (cgroup_path(pid, &version, group, error, size)) return -1;
    if (!version) return 0;
    if (cgroup_mount(version, group, mount, directory, error, size)) return -1;
    for (;;) {
        struct stat info;
        if (stat(directory, &info) != 0 || !S_ISDIR(info.st_mode))
            return memory_error(error, size, "Memory cgroup directory is unavailable");
        uint64_t limit, used;
        int found = cgroup_number(directory, version == 2 ? "memory.max" : "memory.limit_in_bytes", &limit, error, size);
        if (found < 0) return -1;
        if (version == 1 && !found) return memory_error(error, size, "Memory cgroup limit is unavailable");
        if (found && limit != UINT64_MAX) {
            if (cgroup_number(directory, version == 2 ? "memory.current" : "memory.usage_in_bytes", &used, error, size) != 1)
                return memory_error(error, size, "Memory cgroup usage is unavailable");
            *total = smaller(*total, limit);
            *available = smaller(*available, used < limit ? limit - used : 0);
        }
        if (strcmp(directory, mount) == 0) break;
        char *slash = strrchr(directory, '/');
        if (!slash || slash < directory + strlen(mount))
            return memory_error(error, size, "Cannot walk process memory cgroup ancestors");
        *slash = '\0';
    }
    return 0;
}

int acton_perf_memory(int pid, uint64_t *total, uint64_t *available, uint64_t *process,
                      char *error, size_t size) {
    *total = *available = *process = 0;
    FILE *file = fopen("/proc/meminfo", "r");
    if (!file) return memory_error(error, size, "Cannot read /proc/meminfo: %s", strerror(errno));
    char line[256], key[64], unit[8];
    uint64_t kb;
    int found = 0;
    while (fgets(line, sizeof(line), file)) {
        if (sscanf(line, "%63[^:]: %" SCNu64 " %7s", key, &kb, unit) != 3) continue;
        if (strcmp(key, "MemTotal") != 0 && strcmp(key, "MemAvailable") != 0) continue;
        if (strcmp(unit, "kB") != 0 || kb > UINT64_MAX / 1024) {
            fclose(file);
            return memory_error(error, size, "Invalid memory size in /proc/meminfo");
        }
        if (strcmp(key, "MemTotal") == 0) { *total = kb * 1024; found |= 1; }
        else { *available = kb * 1024; found |= 2; }
    }
    int failed = ferror(file);
    fclose(file);
    if (failed || found != 3 || !*total) return memory_error(error, size, "Host memory availability is unavailable");
    *available = smaller(*available, *total);
    if (pid) {
        char path[64];
        snprintf(path, sizeof(path), "/proc/%d/statm", pid);
        file = fopen(path, "r");
        if (!file) return memory_error(error, size, "Cannot read process memory: %s", strerror(errno));
        uint64_t virtual_pages, resident_pages;
        int count = fscanf(file, "%" SCNu64 " %" SCNu64, &virtual_pages, &resident_pages);
        fclose(file);
        long page_size = sysconf(_SC_PAGESIZE);
        if (count != 2 || page_size <= 0 || resident_pages > UINT64_MAX / (uint64_t)page_size)
            return memory_error(error, size, "Invalid process resident memory");
        /* statm RSS is cheap and approximate. It does not include descendants. */
        *process = resident_pages * (uint64_t)page_size;
    }
    return cgroup_memory(pid ? pid : (int)getpid(), total, available, error, size);
}

#elif defined(__APPLE__) && defined(__MACH__)
#include <mach/mach.h>
#include <libproc.h>
#include <sys/resource.h>

int acton_perf_memory(int pid, uint64_t *total, uint64_t *available, uint64_t *process,
                      char *error, size_t size) {
    *total = *available = *process = 0;
    mach_port_t host = mach_host_self();
    host_basic_info_data_t basic;
    mach_msg_type_number_t basic_count = HOST_BASIC_INFO_COUNT;
    vm_statistics64_data_t vm;
    mach_msg_type_number_t vm_count = HOST_VM_INFO64_COUNT;
    vm_size_t page_size;
    kern_return_t result = host_info(host, HOST_BASIC_INFO, (host_info_t)&basic, &basic_count);
    if (result == KERN_SUCCESS) result = host_page_size(host, &page_size);
    if (result == KERN_SUCCESS) result = host_statistics64(host, HOST_VM_INFO64, (host_info64_t)&vm, &vm_count);
    mach_port_deallocate(mach_task_self(), host);
    if (result != KERN_SUCCESS || basic_count < HOST_BASIC_INFO_COUNT || vm_count < HOST_VM_INFO64_REV1_COUNT)
        return memory_error(error, size, "Cannot read host memory: Mach error %d", result);
    if (!basic.max_mem || !page_size) return memory_error(error, size, "Invalid host memory size");
    *total = basic.max_mem;
    /* free_count already includes speculative pages. Subtracting all anonymous
     * pages gives a lower bound on inactive file-backed pages. Do not count
     * anonymous, wired or compressed memory as available, or add purgeable pages
     * which can overlap these queues. This estimate can stop a run early. */
    uint64_t inactive_file = vm.inactive_count > vm.internal_page_count ? vm.inactive_count - vm.internal_page_count : 0;
    *available = smaller(*total, ((uint64_t)vm.free_count + inactive_file) * (uint64_t)page_size);
    if (pid) {
        struct rusage_info_v0 usage;
        if (proc_pid_rusage(pid, RUSAGE_INFO_V0, (rusage_info_t *)&usage) != 0)
            return memory_error(error, size, "Cannot read process footprint: %s", strerror(errno));
        /* Physical footprint includes compressed charges; RSS can fall under
         * pressure even while the process retains its allocations. */
        *process = usage.ri_phys_footprint;
    }
    return 0;
}

#else
int acton_perf_memory(int pid, uint64_t *total, uint64_t *available, uint64_t *process,
                      char *error, size_t size) {
    (void)pid; (void)total; (void)available; (void)process;
    return memory_error(error, size, "Live memory observation requires Linux or macOS");
}
#endif
