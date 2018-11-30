
all: 52 test

52: 52.c runtime.h
	clang -O3 -fomit-frame-pointer -Wall -Wextra -Wpedantic -Wno-gnu -Wno-unused-label $< `pkg-config --libs --cflags glib-2.0` -o $@

52.c: compiler.scm interpreter.scm
	racket $< > $@

clean:
	rm -f 52 52.c core

test:
	cat test.scm | ./52

.PHONY: all clean test
