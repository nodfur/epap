#!/usr/bin/env bash
./zig-out/bin/epap-ft && { pnmtopng frame.pbm > frame.png; }
