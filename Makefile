epap: epap.zig Makefile 
	zig build

build: epap
clean:; rm epap
save:; git save
run:; sudo ./epap > ~/epap-zig.log