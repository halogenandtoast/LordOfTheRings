#!/usr/bin/env bash
# deploy.sh — build both images locally for linux/amd64, push them to Docker
# Hub tagged with the git SHA, then apply k8s/app.yaml to the DOKS cluster.
#
# Requires:
#   - `docker buildx` (default in modern Docker Desktop)
#   - `docker login` already done against Docker Hub
#   - `doctl auth init` done, so the cluster's kubeconfig can be fetched
#   - kubectl
#
# Env overrides:
#   LOTR_DOCKERHUB_USER   Docker Hub namespace (default: halogenandtoast)
#   LOTR_BUILD_PLATFORM   buildx platform (default: linux/amd64)
#   LOTR_CLUSTER          DOKS cluster name (default: lord-of-the-rings)

set -euo pipefail

ROOT_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.." && pwd)"
DOCKER_USER="${LOTR_DOCKERHUB_USER:-halogenandtoast}"
PLATFORM="${LOTR_BUILD_PLATFORM:-linux/amd64}"
CLUSTER="${LOTR_CLUSTER:-lord-of-the-rings}"

SHA="$(git -C "$ROOT_DIR" rev-parse --short HEAD)"
echo "deploy.sh: shipping $SHA"

if ! git -C "$ROOT_DIR" diff --quiet || ! git -C "$ROOT_DIR" diff --cached --quiet; then
  echo "deploy.sh: warning — working tree has uncommitted changes; they are NOT in the built image" >&2
fi

# Make sure a multi-platform-capable builder exists. The default
# `docker-container` driver cross-builds via QEMU.
if ! docker buildx inspect lotr-builder >/dev/null 2>&1; then
  docker buildx create --name lotr-builder --driver docker-container --bootstrap >/dev/null
fi
docker buildx use lotr-builder >/dev/null

build_and_push() {
  local image="$1" ctx="$2"
  echo "deploy.sh: building $image:{$SHA,latest} for $PLATFORM"
  docker buildx build \
    --platform "$PLATFORM" \
    --tag "$image:$SHA" \
    --tag "$image:latest" \
    --push \
    "$ctx"
}

build_and_push "$DOCKER_USER/lotr-backend"  "$ROOT_DIR"
build_and_push "$DOCKER_USER/lotr-frontend" "$ROOT_DIR/frontend"

echo "deploy.sh: applying k8s/app.yaml to $CLUSTER"
KUBECONFIG="$(mktemp)"
export KUBECONFIG
trap 'rm -f "$KUBECONFIG"' EXIT
doctl kubernetes cluster kubeconfig show "$CLUSTER" > "$KUBECONFIG"

sed -e "s/@TAG@/$SHA/g" -e "s|halogenandtoast/|$DOCKER_USER/|g" "$ROOT_DIR/k8s/app.yaml" \
  | kubectl apply -f -
kubectl rollout status deployment/lotr-backend --timeout=5m
kubectl rollout status deployment/lotr-frontend --timeout=5m

echo "deploy.sh: done"
