#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/stat.h>

static char fixture[1024];
static FILE *fixture_fopen(const char *path, const char *mode);

/* The Linux sampler uses only POSIX filesystem APIs, so these fixtures can also
 * exercise its cgroup policy on a macOS development host. */
#ifndef __linux__
#define __linux__ 1
#endif
#define fopen fixture_fopen
#include "perf_memory.c"
#undef fopen

static FILE *fixture_fopen(const char *path, const char *mode) {
    char mapped[2048];
    const char *name = NULL;
    if (strcmp(path, "/proc/meminfo") == 0) name = "meminfo";
    else if (strcmp(path, "/proc/self/mountinfo") == 0) name = "mountinfo";
    else if (strncmp(path, "/proc/", 6) == 0 && strstr(path, "/cgroup")) name = "cgroup";
    else if (strncmp(path, "/proc/", 6) == 0 && strstr(path, "/statm")) name = "statm";
    if (!name) return fopen(path, mode);
    snprintf(mapped, sizeof(mapped), "%s/%s", fixture, name);
    return fopen(mapped, mode);
}

static void write_text(const char *name, const char *text) {
    char path[2048];
    snprintf(path, sizeof(path), "%s/%s", fixture, name);
    FILE *file = fopen(path, "w");
    if (!file || fputs(text, file) < 0 || fclose(file)) abort();
}

static void remove_file(const char *name) {
    char path[2048];
    snprintf(path, sizeof(path), "%s/%s", fixture, name);
    if (unlink(path)) abort();
}

static void make_directory(const char *name) {
    char path[2048];
    snprintf(path, sizeof(path), "%s/%s", fixture, name);
    if (mkdir(path, 0700)) abort();
}

static void mount_table(const char *root, int version) {
    char text[4096], escaped[2048];
    size_t i = 0;
    for (const char *p = fixture; *p; p++) {
        if (*p == ' ') { memcpy(escaped + i, "\\040", 4); i += 4; }
        else escaped[i++] = *p;
    }
    escaped[i] = '\0';
    snprintf(text, sizeof(text), "29 23 0:26 %s %s/cg rw - %s cgroup %s\n",
             root, escaped, version == 2 ? "cgroup2" : "cgroup", version == 2 ? "rw" : "rw,cpu,memory");
    write_text("mountinfo", text);
}

static void expect(const char *name, uint64_t expected_total, uint64_t expected_available) {
    uint64_t total, available, process;
    char error[512];
    int result = acton_perf_memory(0, &total, &available, &process, error, sizeof(error));
    if (result || total != expected_total || available != expected_available || process != 0) {
        fprintf(stderr, "%s: result=%d total=%" PRIu64 " available=%" PRIu64 " error=%s\n",
                name, result, total, available, result ? error : "none");
        exit(1);
    }
}

static void expect_error(const char *name) {
    uint64_t total, available, process;
    char error[512];
    if (!acton_perf_memory(0, &total, &available, &process, error, sizeof(error))) {
        fprintf(stderr, "%s unexpectedly succeeded\n", name);
        exit(1);
    }
}

int main(int argc, char **argv) {
    /* The caller owns this temporary directory and removes it after the test. */
    if (argc != 2 || snprintf(fixture, sizeof(fixture), "%s/cgroup fixtures.XXXXXX", argv[1]) >= (int)sizeof(fixture)) return 2;
    if (!mkdtemp(fixture)) return 2;
    make_directory("cg");
    make_directory("cg/parent");
    make_directory("cg/parent/leaf");
    write_text("meminfo", "MemTotal: 1048576 kB\nMemAvailable: 786432 kB\n");
    write_text("statm", "16 8 4 0 0 0 0\n");
    write_text("cgroup", "0::/parent/leaf\n");
    mount_table("/", 2);
    write_text("cg/parent/memory.max", "536870912\n");
    write_text("cg/parent/memory.current", "503316480\n");
    write_text("cg/parent/leaf/memory.max", "268435456\n");
    write_text("cg/parent/leaf/memory.current", "67108864\n");
    expect("ancestor headroom and child capacity", 268435456, 33554432);
    write_text("cg/parent/leaf/memory.current", "300000000\n");
    expect("usage above limit", 268435456, 0);
    write_text("cg/parent/memory.max", "max\n");
    write_text("cg/parent/leaf/memory.max", "max\n");
    expect("unlimited groups retain host availability", 1073741824, 805306368);
    write_text("cg/parent/leaf/memory.max", "-1\n");
    expect_error("negative limit");
    write_text("cg/parent/leaf/memory.max", "1.5\n");
    expect_error("fractional limit");
    write_text("cg/parent/leaf/memory.max", "268435456\n");
    remove_file("cg/parent/leaf/memory.current");
    expect_error("missing usage");
    write_text("cg/parent/leaf/memory.current", "max\n");
    expect_error("invalid usage");
    write_text("cg/parent/leaf/memory.current", "67108864\n");
    write_text("cg/parent/memory.max", "536870912\n");
    write_text("cgroup", "0::/container/parent/leaf\n");
    mount_table("/container", 2);
    expect("mount root and escaped mountpoint", 268435456, 33554432);
    write_text("cgroup", "0::/\n");
    mount_table("/", 2);
    write_text("cg/memory.max", "67108864\n");
    write_text("cg/memory.current", "16777216\n");
    expect("namespace root still has a limit", 67108864, 50331648);
    write_text("cgroup", "0::/parent/missing\n");
    expect_error("vanished cgroup");
    write_text("cgroup", "0::/../outside\n");
    expect_error("membership outside the namespace");
    write_text("cgroup", "2:cpu,memory:/parent/leaf\n");
    mount_table("/", 1);
    write_text("cg/memory.limit_in_bytes", "9223372036854771712\n");
    write_text("cg/memory.usage_in_bytes", "0\n");
    write_text("cg/parent/memory.limit_in_bytes", "536870912\n");
    write_text("cg/parent/memory.usage_in_bytes", "503316480\n");
    write_text("cg/parent/leaf/memory.limit_in_bytes", "268435456\n");
    write_text("cg/parent/leaf/memory.usage_in_bytes", "67108864\n");
    expect("v1 ancestor limits", 268435456, 33554432);
    write_text("cgroup", "1:name=systemd:/\n");
    expect("no memory controller", 1073741824, 805306368);
    remove_file("cgroup");
    expect_error("unreadable cgroup membership");
    write_text("cgroup", "1:name=systemd:/\n");

    /* Check the join directly: stat() would hide a truncated path later. */
    mount_table("/", 2);
    char group[PATH_MAX], mount[PATH_MAX], directory[PATH_MAX], error[512];
    size_t group_length = PATH_MAX - 1 - (strlen(fixture) + 3);
    memset(group, 'x', sizeof(group));
    group[0] = '/';
    group[group_length] = '\0';
    if (cgroup_mount(2, group, mount, directory, error, sizeof(error))
        || strlen(directory) != PATH_MAX - 1
        || strcmp(directory + strlen(mount), group) != 0) abort();
    group[group_length] = 'x';
    group[group_length + 1] = '\0';
    strcpy(mount, "unchanged");
    strcpy(directory, "unchanged");
    if (!cgroup_mount(2, group, mount, directory, error, sizeof(error))
        || strcmp(mount, "unchanged") || strcmp(directory, "unchanged")) abort();

    write_text("meminfo", "MemTotal: 1048576 kB\n");
    expect_error("missing MemAvailable");
    printf("native Linux memory fixtures passed (%s)\n", fixture);
    return 0;
}
