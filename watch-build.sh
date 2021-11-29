#!/usr/bin/env bash
set -ex
git ls-files | entr -cr make yolo
