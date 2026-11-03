#!/usr/bin/env python3
"""Exercise verified toolchain installation without downloading a compiler."""
import hashlib
import io
import os
from pathlib import Path
import subprocess
import tarfile
import tempfile
import unittest

INSTALL = Path(__file__).resolve().parents[1] / 'deps/zig/install.sh'


class InstallTests(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp.cleanup)
        self.root = Path(self.temp.name)
        self.cache = self.root / 'cache'
        self.dist = self.root / 'dist/zig'
        self.extra = self.root / 'extras'
        self.extra.mkdir()
        (self.extra / 'extra.txt').write_text('extra')
        self.manifest = self.root / 'SHA256SUMS'
        self.manifest.write_text('')

    def fixture(self, version, reported=None):
        name = f'zig-test-{version}'
        path = self.root / (name + '.tar.xz')
        with tarfile.open(path, 'w:xz') as tar:
            files = {'zig': f'#!/bin/sh\necho "{reported or version}"\n',
                     'lib/std/std.zig': version}
            for relative, value in files.items():
                data = value.encode()
                info = tarfile.TarInfo(name + '/' + relative)
                info.size = len(data)
                info.mode = 0o755 if relative == 'zig' else 0o644
                tar.addfile(info, io.BytesIO(data))
        with self.manifest.open('a') as out:
            out.write(hashlib.sha256(path.read_bytes()).hexdigest() + '  ' + path.name + '\n')
        return path

    def install(self, archive, version, success=True, env=None):
        result = subprocess.run(['sh', str(INSTALL), str(self.cache / archive.name),
                                 archive.as_uri(), str(self.manifest), version,
                                 str(self.dist), str(self.extra)], capture_output=True, text=True, env=env)
        self.assertEqual(result.returncode == 0, success, result.stdout + result.stderr)
        return result

    def test_download_and_clean_version_switch_with_cached_archives(self):
        one, two = self.fixture('one'), self.fixture('two')
        self.install(one, 'one')
        (self.dist / 'obsolete').write_text('old file')
        self.install(two, 'two')
        self.assertFalse((self.dist / 'obsolete').exists())
        self.assertFalse((self.dist / '.version-one').exists())
        self.install(one, 'one')
        self.assertEqual((self.dist / 'lib/std/std.zig').read_text(), 'one')
        self.assertTrue((self.dist / '.version-one').exists())
        self.assertEqual((self.dist / 'extra.txt').read_text(), 'extra')

    def test_corrupt_cached_archive_leaves_installation(self):
        archive = self.fixture('one')
        self.install(archive, 'one')
        (self.cache / archive.name).write_bytes(b'corrupt')
        self.install(archive, 'one', success=False)
        self.assertEqual((self.dist / 'lib/std/std.zig').read_text(), 'one')

    def test_download_checksum_failure_does_not_poison_cache(self):
        archive = self.fixture('one')
        archive.write_bytes(b'truncated transfer')
        self.install(archive, 'one', success=False)
        self.assertFalse((self.cache / archive.name).exists())
        self.assertFalse(self.dist.exists())
        self.assertEqual(list(self.cache.iterdir()), [])

    def test_failed_download_keeps_previous_installation(self):
        one, two = self.fixture('one'), self.fixture('two')
        self.install(one, 'one')
        tools = self.root / 'tools'
        tools.mkdir()
        curl = tools / 'curl'
        curl.write_text('#!/bin/sh\nexit 56\n')
        curl.chmod(0o755)
        env = {**os.environ, 'PATH': str(tools) + os.pathsep + os.environ['PATH']}
        self.install(two, 'two', success=False, env=env)
        self.assertFalse((self.cache / two.name).exists())
        self.assertEqual((self.dist / 'lib/std/std.zig').read_text(), 'one')

    def test_wrong_version_keeps_previous_installation(self):
        one, two = self.fixture('one'), self.fixture('two', reported='unexpected')
        self.install(one, 'one')
        self.install(two, 'two', success=False)
        self.assertEqual((self.dist / 'lib/std/std.zig').read_text(), 'one')
        self.assertEqual(list(self.dist.parent.glob('zig.stage.*')), [])

    def test_missing_manifest_entry_fails_before_download(self):
        archive = self.fixture('one')
        self.manifest.write_text('')
        self.install(archive, 'one', success=False)
        self.assertFalse(self.cache.exists())


if __name__ == '__main__':
    unittest.main()
