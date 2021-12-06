;;
;; Load foreign symbols from FreeType, Harfbuzz, libcap, etc.
;;
;; They're all built into one shared library file using Zig.
;;

(in-package :epap)

(define-foreign-library epapi
  (:unix (:or "./zig-out/lib/libepapi.so"
              "./zig-out/lib/libepapi.dylib")))

(use-foreign-library epapi)
