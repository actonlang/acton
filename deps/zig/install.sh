#!/bin/sh
# Download and install one pinned, verified Zig distribution.
set -eu

archive=$1
url=$2
manifest=$3
version=$4
destination=$5
extras=$6
filename=${archive##*/}
expected=$(awk -v file="$filename" '$2 == file { print $1 }' "$manifest")
case "$expected" in ''|*[!0-9a-f]*) echo "Missing or invalid Zig checksum for $filename" >&2; exit 1 ;; esac
[ "${#expected}" -eq 64 ] || { echo "Invalid Zig checksum for $filename" >&2; exit 1; }

hash_file() {
    if command -v sha256sum >/dev/null 2>&1; then
        sha256sum "$1" | awk '{ print $1 }'
    else
        shasum -a 256 "$1" | awk '{ print $1 }'
    fi
}
verify() {
    actual=$(hash_file "$1")
    [ "$actual" = "$expected" ] || { echo "Zig checksum mismatch: $1" >&2; exit 1; }
}

mkdir -p "$(dirname "$archive")" "$(dirname "$destination")"
partial=
staging=
backup=
cleanup() {
    [ -z "$partial" ] || rm -f "$partial"
    [ -z "$staging" ] || rm -rf "$staging"
    if [ -n "$backup" ] && [ -d "$backup/previous" ] && [ ! -e "$destination" ]; then
        mv "$backup/previous" "$destination"
    fi
    [ -z "$backup" ] || rm -rf "$backup"
}
trap cleanup EXIT
trap 'exit 1' HUP INT TERM
if [ ! -f "$archive" ]; then
    partial=$(mktemp "$archive.download.XXXXXX")
    curl --fail --location --retry 5 --retry-delay 2 --retry-max-time 120 --retry-connrefused -o "$partial" "$url"
    verify "$partial"
    mv "$partial" "$archive"
    partial=
fi
# Cached files are checked immediately before every extraction too.
verify "$archive"
staging=$(mktemp -d "$destination.stage.XXXXXX")
tar Jxf "$archive" --strip-components=1 -C "$staging"
[ -x "$staging/zig" ] && [ -f "$staging/lib/std/std.zig" ] || { echo 'Invalid Zig archive layout' >&2; exit 1; }
actual_version=$(env -u ZIG_LIB_DIR "$staging/zig" version)
[ "$actual_version" = "$version" ] || { echo "Unexpected Zig version: $actual_version" >&2; exit 1; }
rm -rf "$staging/doc"
cp -a "$extras/." "$staging/"
touch "$staging/.version-$version"
backup=$(mktemp -d "$destination.old.XXXXXX")
if [ -e "$destination" ]; then mv "$destination" "$backup/previous"; fi
mv "$staging" "$destination"
staging=
rm -rf "$backup"
backup=
