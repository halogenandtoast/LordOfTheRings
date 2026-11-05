#!/usr/bin/env python3
"""Profile one module against the installed library without touching its artifacts.

Run `stack build` first. --source-root can point at a compatible older checkout
for a Generic/TH comparison using the same compiler and installed dependencies.
"""
import argparse
import json
from pathlib import Path
import re
import subprocess
import time


def main():
    root = Path(__file__).resolve().parents[1]
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('module')
    parser.add_argument('--source-root', type=Path, default=root)
    parser.add_argument('--output-directory', type=Path, required=True)
    parser.add_argument('--ghc-option', action='append', default=[])
    args = parser.parse_args()
    if not re.fullmatch(r'[A-Z]\w*(?:\.[A-Z]\w*)*', args.module):
        parser.error('expected a Haskell module name')
    output = args.output_directory.resolve()
    output.mkdir(parents=True, exist_ok=True)
    cabal = (root / 'LordOfTheRings.cabal').read_text()
    extensions = cabal.split('  default-extensions:\n', 1)[1].split('  ghc-options:', 1)[0].split()
    source = args.source_root.resolve() / 'src' / Path(args.module.replace('.', '/')).with_suffix('.hs')
    command = ['stack', 'exec', '--', 'ghc', '-c', str(source), '-package', 'LordOfTheRings',
               '-O1', '-fforce-recomp', '-ddump-timings', '-odir', str(output), '-hidir', str(output),
               '-XGHC2021'] + ['-X' + extension for extension in extensions]
    command += args.ghc_option + ['+RTS', '-s', '-RTS']
    start = time.monotonic()
    with (output / 'ghc.log').open('w') as log:
        log.write(json.dumps(command) + '\n')
        log.flush()
        result = subprocess.run(command, cwd=root, stdout=log, stderr=subprocess.STDOUT)
        log.write(f'\nWALL_SECONDS={time.monotonic() - start:.3f} EXIT={result.returncode}\n')
    print(output / 'ghc.log')
    raise SystemExit(result.returncode)


if __name__ == '__main__':
    main()
