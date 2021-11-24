all:
	zig build-exe epap.zig -lc \
	  vendor/bcm2835-1.70/src/bcm2835.c -I vendor/bcm2835-1.70/src
