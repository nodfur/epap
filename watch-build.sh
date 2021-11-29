#!/usr/bin/env bash
set -ex
ls | entr -cr make yolo
