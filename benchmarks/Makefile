.PHONY: run clean

./build/%.s: %.ml
	mkdir -p ./build/
	stack run -- -i $^ -o $@ --emit-all

run: ./build/$(EXEC).s
	rm -rf ./*.txt
	./r72b -input $^
	./simulation/sim

clean:
	rm -f ./build/*.s
	rm -rf ./build/*.ml
	rm -f out.*
	rm -rf ./print_int.txt
