.PHONY: run clean
.PRECIOUS: %.s

ASSEMBLER = ./bin/r72b
SIMULATOR = ./bin/r7sim
OUTPUT = ./print_int.txt
EXEC = fib
OPTION =

%.s: %.ml
	mkdir -p ./build/
	stack run -- -i $^ -o ./build/$@ --emit-all --verbose $(OPTION)
	cp ./build/$@ .

%.bin: %.s
	mkdir -p ./build/
	$(ASSEMBLER) -input $^ -output ./build/$(basename $@ .bin)
	cp ./build/$@ .

run: $(EXEC).bin
	rm -f $(OUTPUT)
	echo 2 | $(SIMULATOR) -i $^

clean:
	rm -rf ./build/
	rm -f *.s
	rm -f *.bin
	rm -f $(OUTPUT)
