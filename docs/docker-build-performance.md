# Docker compilation study — 2026-11-05

## Scope and environment

Implementation commits: `fe984023` (Docker caches), `f45ed879` (JSON codecs),
`8f88a4e6` (engine split and normal optimization), and `45691bfb` (API object
retention during recovery), plus `539ea380` (measured ally registry boundary). The starting revision was
`eab5cf23`; the baseline was refreshed through `aec2b6c5` to include another
session's independent card changes. Subsequent concurrent card edits are outside
these measurements. There was no `AGENTS.md` in this checkout or its ancestors.

The reference was MarvelChampions commits `d6f59caf8c`, `e3d08adf9b`, and
`0752b416b2`. No code, flags, or module boundaries were assumed transferable.
Measurements used GHC 9.14.1, a 20-core arm64 Mac with 128 GiB RAM, and a dedicated
`lotr-build-study` docker-container builder on OrbStack. Docker targeted
`linux/amd64` under emulation; compiler phase profiles ran natively on arm64.
Images were loaded locally, never pushed or deployed. The already-running
independent deployment and its builder were left alone.

## Findings and changes

* Existing cache mounts had architecture-independent IDs and no exclusive locking.
  Both dependency and application steps now mount the same architecture/GHC/version
  namespaces with `sharing=locked`.
* `COPY . .` mixed compilation with documentation, tests, infrastructure, tools,
  and runtime data. Compilation now copies only descriptors, `src`, `app`, and
  the two build helpers. `cards.json` is copied in the runtime stage only.
* `.dockerignore` excluded `.stack-work/` but admitted twelve `.stack-work-*`
  trees totaling roughly 7 GiB on this workstation. The wildcard exclusion now
  covers them; `dist-newstyle` is also excluded.
* A dirty marker survives failed or interrupted application builds. Recovery
  checks interface bounds, object section bounds, object/interface timestamps,
  and all static/dynamic ways in one Python process. Valid completed modules,
  including those produced during the interrupted run, are retained. Boot
  objects legitimately have zero length and are handled separately. Potentially
  partial package archives and executable outputs are recreated; completed API module objects are retained.
* Per-file `ghc --show-iface` validation was rejected after an emulated run spent
  seconds per interface. The batched replacement checked 7,320 real Linux
  artifact pairs in about 1.1 seconds and removed no completed modules. Its
  version-specific header checks follow GHC's
  [interface writer](https://github.com/ghc/ghc/blob/ghc-9.14.1-release/compiler/GHC/Iface/Binary.hs).
* The optimized amd64 cache mounts measured 625 MB for Stack and 1.754 GB for
  application artifacts. The daemon configuration reserves 12 GB for cache
  mounts, with a 24 GB ceiling and 90-day age policy; the general policy reserves
  24 GB, caps usage at 48 GB, and requests 15 GB free space. This leaves headroom
  for multiple architectures/compiler versions. The old builder's roughly
  106 GB of accumulated layers were left untouched; this does not reclaim
  its existing storage.

Recovery targets interrupted/truncated writes, not arbitrary disk corruption.
It checks the GHC 9.14.1 interface format and ELF64 objects for the supported
amd64/arm64 Linux targets. Update the validator as well as the image and cache
namespace when upgrading GHC. Stack remains responsible for dependency package
registration and rebuilding unfinished dependency installations.

## Compiler profiles

Each source module was forced through `ghc -c -O1 -ddump-timings +RTS -s -RTS`
against the same installed package/dependencies, with the package's language
extensions and separate output directories. Times below are GHC elapsed time;
allocation is cumulative, not live memory. Concurrent machine activity means
these are diagnostic measurements rather than isolated microbenchmarks.

| Module | Generic time | Generated time | Generic allocation | Generated allocation | Generic peak residency | Generated peak residency |
| --- | ---: | ---: | ---: | ---: | ---: | ---: |
| Message | 374.4 s | 41.8 s | 351.8 GB | 41.7 GB | 10.85 GB | 0.66 GB |
| Modifier | 264.6 s | 22.4 s | 294.3 GB | 22.5 GB | 5.18 GB | 0.49 GB |
| Window | 50.5 s | 8.8 s | 49.4 GB | 8.7 GB | 0.87 GB | 0.16 GB |

One Message simplifier pass allocated 114.2 GB; a Modifier pass allocated 85.6 GB.
Message spent 267.8 seconds in the mutator and 89.5 seconds in GC (about 25% of
CPU time): this was active optimization with heavy allocation, not an idle
linker or runtime deadlock. Allocation totals should not be confused with RSS.
The state record module `Game.Base` took 7.6 seconds, allocated 4.0 GB, and peaked
at 122 MB. Its existing codecs were retained. The expensive sums use Aeson TH
with unchanged `defaultOptions`; related mutually recursive records are generated
in the same declaration group. The escape parser still supplies its legacy
`escapeParty` default; the drawing-source and saved-game defaults are unchanged.
`Generic` derivations are retained, including for the independent compatibility
oracles.

The original complete `Game` module took 38.8 seconds and allocated 63.05 GB.
Adding only `NOINLINE runGameMessage` took 37.8 seconds and allocated the same
63.05 GB. That difference did not justify keeping a pragma. The loop is already
concrete in `GameT`; no polymorphic monad specialization disaster comparable to
Marvel's was demonstrated. A separate measured boundary, `NOINLINE allAllies`, prevents the ally registry
from exporting card dictionaries in its unfolding. A real card implementation
edit then rebuilt two modules instead of 33, reducing the optimized Docker
rebuild from 141.9 to 63.6 seconds. This boundary is retained; no whole engine
module has optimization disabled. Normal Cabal optimization replaces the former global
local-package `-O0`. Existing parallel compilation and compiler RTS settings are
preserved. The compatibility test module alone uses `-O0`: optimizing its old
Generic oracle reproduced minutes of compilation and about 14 GB residency.
It is excluded from Docker and does not affect engine optimization.

Reproduce individual profiles after `stack build` has installed the library:

```sh
python3 scripts/profile-haskell-module.py LordOfTheRings.Message \
  --output-directory /tmp/lotr-message-profile
# Use --source-root /path/to/compatible/old/checkout for a comparison.
```

The helper saves the exact invocation, phase timings, RTS statistics, and wall
clock time. It uses separate outputs and does not overwrite Stack objects. Do
not run it against a package database while another build unregisters that package.

## Engine boundaries

`Game` remains the public facade. Existing `Game.Base`, `GameT`, matcher/selector,
and helper modules were retained. Setup, loop control, game-ending checks,
modifiers, separated areas, entity dispatch, card actions, combat queries,
payment, serialization, queries, and message execution now have separate modules.
The central message case expression is still large; it was moved intact rather
than changing its routing semantics. No queue pushes, traversal order, randomness,
or game rules were rewritten. The existing `Game.Base.hs-boot` still declares the
type implemented by `Game.Base`; no boot declaration was assigned to a facade.

## Validation

* Original baseline: 4,385 passing tests initially; 4,388 after the independent
  card commits were included. Optimized tree: 4,394 passing tests, including six
  new Generic/TH compatibility cases. No remaining test failure was observed.
* Compatibility checks cover both migration directions, `toJSON` and direct
  `toEncoding`, constructor tags, positional contents, modifier and window
  records, nested messages/questions, Unicode/escaping, optional values, live
  game field names, and legacy drawing, escape, and saved-game defaults.
* `tools/BuildSmoke.hs` runs Passage Through Mirkwood, Journey Along the Anduin,
  and Escape from Dol Guldur with seeds 1, 7, and 42. Both before and after the registry boundary, all 720 snapshots, queues,
  RNG states, and logs matched byte-for-byte across original and optimized builds.
  Each trace file was 7,660,895 bytes; SHA-256:
  `06340658d23ba5b1f73d4ae9301cf9214877e49c75d19dc7292f3767fe2d11bb`.
* Ten recovery tests exercise completed modules, zero-byte boot objects, missing
  or stale objects, malformed interfaces, nonempty truncated objects, truncated
  ELF section tables, truncated interface tails, retained dependency archives and API modules,
  and retry markers.
* The original and optimized amd64 images built and linked with normal parallel settings. `ldd` resolved
  all runtime libraries. A disposable PostgreSQL 17 database on an internal-only
  network accepted migrations; the final measured image served `/api/cards`
  with HTTP 200 and its bundled card data. The original image returned HTTP 200
  against the same isolated database. No production service was contacted.
  A first startup raced database readiness; restarting after `pg_isready` fixed it.
* A separate pre-existing deployment also completed with parallel linking. No
  Rosetta linking hang was reproduced, so no executable-wide `-j1` was added.

Run the local checks with:

```sh
python3 -m unittest discover -s scripts/tests -v
stack test --ta '--seed 1 +RTS -N2 -RTS'
stack exec -- runghc tools/BuildSmoke.hs > /tmp/lotr-trace.json
```

## Warm Docker measurements

Both variants were built once in separate contexts before these sequential
measurements. Each build used `--platform linux/amd64 --load`, the same builder,
and a local image tag. Times include buildx overhead and image loading.

| Input change | Original (`-O0`) | Optimized before registry boundary | Haskell modules before boundary |
| --- | ---: | ---: | ---: |
| None | 2.724 s | 3.480 s | 0 |
| README text | 8.378 s | 2.694 s | 0 |
| Frontend TypeScript | 2.564 s | 2.578 s | 0 |
| Runtime cards.json whitespace | 9.057 s | 2.780 s | 0 |
| Snowbourn Scout behavior | 47.318 s | 141.918 s | 33 |

The targeted registry boundary reduced the last row to **63.621 seconds and two
modules** (SnowbournScout and Ally), with normal engine optimization retained.
This is a 55% reduction from the first optimized result, but still 16.3 seconds
slower than the old unoptimized build. No-change/frontend differences are within
ordinary run-to-run overhead; the original frontend was already excluded.

The original Dockerfile was used with only cache IDs/locking adjusted to keep
its application artifacts isolated and reuse the already-built dependencies.
Its fresh application compilation took 620.2 seconds. A comparable successful
fresh optimized compilation was not timed; its initial compile was followed by
import fixes and cache recovery, so that duration would not be a fair cold-build
comparison. The controlled baseline context did not contain the workstation's
extra `.stack-work-*` trees; it therefore does not quantify their old upload cost.

The card edit changed `placeProgress a location 1` to
`placeProgress a location 2`: its response would place two progress instead of
one. This changed executable behavior and generated interface/object contents;
it was not a comment edit. It existed only in the benchmark context. The
rebuild included card registries, dependent engine modules, and API linking.
Restoring the original source/runtime data reused an earlier complete image
layer: 4.274 seconds before the boundary and 4.805 seconds after it. These were
cache hits, not measurements of reverse recompilation.

These warm measurements do not imply a card edit compiles only one module or
that a fresh optimized build is faster than the former unoptimized build.
The ally boundary stops this measured cascade; other card registries remain
unmeasured and may still propagate implementation changes. The central message
case expression also remains large. Archiving and linking still contribute to
the remaining incremental-build cost. The large sum profiles
isolate the optimization improvement; the Docker measurements establish which
inputs actually invalidate compilation. Only amd64 Docker execution was validated;
arm64 cache naming and ELF support were inspected but no arm64 image was built.

Raw logs and trace files from this session are under `/tmp/lotr-build-study` on
the workstation. The benchmark builder and its caches were retained for reuse.
