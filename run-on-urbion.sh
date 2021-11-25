#!/usr/bin/env bash
set -ex
make build
git save
git push
ssh urbion "cd nodfur/epap && git pull && make build run"
