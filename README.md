# LordOfTheRings

The backend builds with Stack and GHC 9.14.1. Docker compilation uses locked,
persistent caches separated by target architecture and compiler version. Keep the
same BuildKit builder between builds; its cache mounts are not stored in the image.

For a local image build without pushing or deploying:

```sh
docker buildx create --name lotr-builder-v2 --driver docker-container \
  --buildkitd-config scripts/buildkitd.toml --bootstrap
docker buildx build --builder lotr-builder-v2 --platform linux/amd64 \
  --load -t lotr-backend:local .
```

Create the builder once. `scripts/deploy.sh` uses this builder by default;
`LOTR_BUILDER` selects another existing builder. Changing a daemon configuration
file does not reconfigure an existing builder. Existing builders and their caches
are left intact. `BUILD_CACHE_VERSION` provides an explicit new cache namespace
when needed; routine failures do not require changing it or pruning caches.

See [Docker compilation measurements and validation](docs/docker-build-performance.md)
for the profiling method, compatibility checks, warm-build results, and limitations.
