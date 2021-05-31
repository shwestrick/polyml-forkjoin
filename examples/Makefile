
all: fib dmm primes

.PHONY: phony

phony:


fib: phony
	mkdir -p bin
	polyc -o bin/fib fib.sml

dmm: phony
	mkdir -p bin
	polyc -o bin/dmm dmm.sml

primes: phony
	mkdir -p bin
	polyc -o bin/primes primes.sml

clean:
	rm -rf bin
