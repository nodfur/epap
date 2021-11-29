epap: epap.zig Makefile 
	zig build --verbose

build: epap
clean:; rm epap
save:; git save
test:; ./run-test.sh
yolo: epap test save
