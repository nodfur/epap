epap: epap.zig Makefile 
	zig build --verbose

build: epap
clean:; rm epap
save:; git save && git push
test:; ./run-test.sh
yolo: epap test save
