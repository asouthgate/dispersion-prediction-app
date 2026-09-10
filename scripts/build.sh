#!/bin/bash
set -euo pipefail
export DOCKER_BUILDKIT=1

FLAGS=("$@")

docker build "${FLAGS[@]}" -t dispersion-prediction-app-frontend -f docker/Dockerfile.frontend .
docker build "${FLAGS[@]}" -t dispersion-prediction-app-api      -f docker/Dockerfile.backend .
docker build "${FLAGS[@]}" -t dispersion-prediction-app-postgis  -f docker/Dockerfile.gis .
