#!/usr/bin/env python3
"""Recover GHC 9.14.1 Linux artifacts under an exclusively locked BuildKit mount.

Check lengths/section bounds in one process: launching GHC for every interface is
prohibitively expensive under Rosetta. This detects interrupted/truncated writes,
not arbitrary bit rot. GHC still checks interface fingerprints on the next build.
"""
from pathlib import Path
import struct
import sys


def complete_interface(path):
    # GHC/Iface/Binary.hs: magic, version string, way string, then relative
    # pointers to extension fields, FastStrings, Names and IfaceTypes. The
    # extension fields are written last, so their pointer detects a cut-off tail.
    data = path.read_bytes()
    if data[:9] != b'\x01\xfa\xce\x64\x049141':
        return False
    pos = 9
    if len(data) <= pos:
        return False
    way_length = data[pos]
    if way_length not in (0, 3) or data[pos + 1:pos + 1 + way_length] not in (b'', b'dyn'):
        return False
    pos += 1 + way_length
    if len(data) < pos + 16:
        return False
    pointers = [at + struct.unpack_from('>I', data, at)[0] for at in range(pos, pos + 16, 4)]
    return all(pos + 16 <= target < len(data) for target in pointers)


def complete_object(path):
    # Both deployment targets use ELF64 little-endian relocatable objects.
    # Check every file-backed section, including the section table itself. A
    # nonempty file or ELF header alone accepts interrupted compiler output.
    data = path.read_bytes()
    if len(data) < 64 or data[:6] != b'\x7fELF\x02\x01':
        return False
    if struct.unpack_from('<H', data, 16)[0] != 1:
        return False
    offset = struct.unpack_from('<Q', data, 40)[0]
    entry_size, count = struct.unpack_from('<HH', data, 58)
    if entry_size != 64 or offset < 64 or not count or offset + count * entry_size > len(data):
        return False
    for index in range(count):
        section = offset + index * entry_size
        kind = struct.unpack_from('<I', data, section + 4)[0]
        start, size = struct.unpack_from('<QQ', data, section + 24)
        if kind != 8 and start + size > len(data):  # SHT_NOBITS has no file contents
            return False
    return True


def recover(work_dir, checkpoint):
    cutoff = checkpoint.stat().st_mtime_ns
    removed = set()
    checked = 0
    for interface in (work_dir / 'dist').rglob('*'):
        if not interface.is_file():
            continue
        suffix = '-boot' if interface.name.endswith('-boot') else ''
        extension = '.dyn_hi' + suffix if interface.name.endswith('.dyn_hi' + suffix) else '.hi' + suffix
        if not interface.name.endswith(extension):
            continue
        stem = str(interface)[:-len(extension)]
        if (stem, suffix) in removed:
            continue
        obj = Path(str(interface).replace(extension, extension.replace('hi', 'o')))
        valid = obj.is_file() and interface.stat().st_size > 0
        changed = interface.stat().st_mtime_ns > cutoff or (obj.exists() and obj.stat().st_mtime_ns > cutoff)
        if valid and not suffix:
            valid = obj.stat().st_size > 0 and obj.stat().st_mtime_ns >= interface.stat().st_mtime_ns
        if valid and changed:
            checked += 1
            valid = complete_interface(interface) and (bool(suffix) or complete_object(obj))
        if not valid:
            print(f'Removing incomplete module: {stem}{suffix}', flush=True)
            for way in ('.hi', '.o', '.dyn_hi', '.dyn_o'):
                Path(stem + way + suffix).unlink(missing_ok=True)
            removed.add((stem, suffix))
    print(f'Recovery checked {checked} changed pairs; removed {len(removed)} incomplete modules', flush=True)


if __name__ == '__main__':
    recover(Path(sys.argv[1]), Path(sys.argv[2]))
