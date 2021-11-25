epap: epap.zig Makefile 
	zig build-exe epap.zig -lc \
	  -fno-stack-check -fno-sanitize-c \
	  vendor/bcm2835-1.70/src/bcm2835.c -I vendor/bcm2835-1.70/src

build: epap
clean:; rm epap
save:; git save
yolo:; make build && make save

watch:; ls | entr -r make yolo