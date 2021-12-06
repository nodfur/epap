build:; zig build --verbose
clean:; rm epap
save:; git save && git push
test:; ./run-test.sh
yolo: build test save
