#!/bin/sh
set -ex
echo ";;; Launching the EPAP Emacs environment..."
nix develop -c ./bin/epap-emacs
