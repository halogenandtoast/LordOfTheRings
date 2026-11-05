"""Recovery contract tests; binary fixtures exercise validation; only Stack is stubbed."""
import os
from pathlib import Path
import subprocess
import struct
import tempfile
import unittest

SCRIPT = Path(__file__).resolve().parents[1] / "docker-build-backend.sh"


class RecoveryTest(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp.cleanup)
        self.root = Path(self.temp.name)
        self.work = self.root / "work"
        self.dist = self.work / "dist/x86_64-linux/ghc-9.14.1/build"
        self.dist.mkdir(parents=True)
        self.bin = self.root / "bin"
        self.bin.mkdir()
        self.log = self.root / "stack-args"
        self.env = dict(os.environ, PATH=f"{self.bin}:{os.environ['PATH']}",
                        LOTR_BUILD_WORK_DIR=str(self.work),
                        LOTR_BUILD_OUTPUT_DIR=str(self.root / 'out'),
                        STACK_LOG=str(self.log))
        self.command('stack', 'printf "%s\\n" "$@" > "$STACK_LOG"\nexit "${STACK_EXIT:-0}"')
        self.dirty = self.work / '.docker-build-in-progress'

    def command(self, name, body):
        path = self.bin / name
        path.write_text('#!/bin/sh\n' + body + '\n')
        path.chmod(0o755)

    def pair(self, name, *, timestamp=200, interface=None, obj=None, boot=False):
        if interface is None:
            interface = b'\x01\xfa\xce\x64\x049141\x00' + struct.pack('>IIII', 16, 12, 8, 4) + b'\x00'
        if obj is None:
            obj = bytearray(128)
            obj[:6] = b'\x7fELF\x02\x01'
            struct.pack_into('<H', obj, 16, 1)
            struct.pack_into('<Q', obj, 40, 64)
            struct.pack_into('<HH', obj, 58, 64, 1)
            obj = b'' if boot else bytes(obj)
        suffix = '-boot' if boot else ''
        paths = []
        for extension, contents in [('hi', interface), ('o', obj), ('dyn_hi', interface), ('dyn_o', obj)]:
            p = self.dist / f'{name}.{extension}{suffix}'
            p.parent.mkdir(parents=True, exist_ok=True)
            p.write_bytes(contents if isinstance(contents, bytes) else contents.encode())
            os.utime(p, (timestamp, timestamp))
            paths.append(p)
        return paths

    def interrupted(self):
        self.dirty.touch()
        os.utime(self.dirty, (100, 100))

    def run_build(self, **env):
        return subprocess.run(['sh', str(SCRIPT)], env=dict(self.env, **env),
                              capture_output=True, text=True)

    def test_preserves_completed_modules_even_from_interrupted_run(self):
        old = self.pair('Old', timestamp=50)
        completed = self.pair('Complete')
        boot = self.pair('Boot', boot=True)
        self.interrupted()
        result = self.run_build()
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertTrue(all(p.exists() for p in old + completed + boot))
        self.assertIn('--force-dirty', self.log.read_text())
        self.assertFalse(self.dirty.exists())

    def test_preserves_completed_api_modules_and_relinks_executable(self):
        paths = self.pair('LordOfTheRings-exe/LordOfTheRings-exe-tmp/Main')
        executable = self.dist / 'LordOfTheRings-exe/LordOfTheRings-exe'
        executable.write_text('possibly incomplete executable')
        self.interrupted()
        self.assertEqual(self.run_build().returncode, 0)
        self.assertTrue(all(p.exists() for p in paths))
        self.assertFalse(executable.exists())

    def test_rejects_nonempty_truncated_object_and_all_its_ways(self):
        broken = self.pair('Truncated', obj='NONEMPTY BUT TRUNCATED')
        self.interrupted()
        self.assertEqual(self.run_build().returncode, 0)
        self.assertTrue(all(not p.exists() for p in broken))

    def test_rejects_truncated_elf_with_intact_magic_and_header(self):
        paths = self.pair('TruncatedSectionTable')
        paths[1].write_bytes(paths[1].read_bytes()[:-1])
        os.utime(paths[1], (200, 200))
        self.interrupted()
        self.assertEqual(self.run_build().returncode, 0)
        self.assertTrue(all(not p.exists() for p in paths))

    def test_rejects_truncated_interface_with_intact_magic(self):
        paths = self.pair('TruncatedInterface')
        paths[0].write_bytes(paths[0].read_bytes()[:-1])
        os.utime(paths[0], (200, 200))
        self.interrupted()
        self.assertEqual(self.run_build().returncode, 0)
        self.assertTrue(all(not p.exists() for p in paths))

    def test_rejects_incomplete_interface_and_missing_object(self):
        broken = self.pair('BadInterface', interface='BAD')
        missing = self.pair('MissingObject')
        missing[1].unlink()
        self.interrupted()
        self.assertEqual(self.run_build().returncode, 0)
        self.assertTrue(all(not p.exists() for p in broken + missing))

    def test_rejects_object_older_than_new_interface(self):
        paths = self.pair('StaleObject')
        os.utime(paths[1], (150, 150))
        self.interrupted()
        self.assertEqual(self.run_build().returncode, 0)
        self.assertTrue(all(not p.exists() for p in paths))

    def test_preserves_dependencies_and_old_archives(self):
        dependency = self.work / 'install/dependency.a'
        dependency.parent.mkdir()
        dependency.write_text('old archive')
        os.utime(dependency, (50, 50))
        recent = self.dist / 'libRecent.a'
        recent.write_text('partial archive')
        self.interrupted()
        self.assertEqual(self.run_build().returncode, 0)
        self.assertTrue(dependency.exists())
        self.assertFalse(recent.exists())

    def test_failure_retains_marker_and_next_run_reuses_complete_pairs(self):
        self.assertEqual(self.run_build(STACK_EXIT='9').returncode, 9)
        self.assertTrue(self.dirty.exists())
        complete = self.pair('Complete', timestamp=self.dirty.stat().st_mtime + 2)
        self.assertEqual(self.run_build().returncode, 0)
        self.assertTrue(all(p.exists() for p in complete))
        self.assertFalse(self.dirty.exists())

    def test_clean_run_does_not_force_recompile(self):
        self.assertEqual(self.run_build().returncode, 0)
        args = self.log.read_text()
        self.assertNotIn('--force-dirty', args)
        self.assertIn('LordOfTheRings:exe:LordOfTheRings-exe', args)


if __name__ == '__main__':
    unittest.main()
