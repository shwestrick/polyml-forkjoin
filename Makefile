
.PHONY: fib

fib:
	mkdir -p bin
	polyc -o bin/fib fib.sml

clean:
	rm -rf bin
