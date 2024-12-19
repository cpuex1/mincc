.PHONY: run clean
.PRECIOUS: %.s

ASSEMBLER = ./bin/r72b
SIMULATOR = ./bin/r7sim
OUTPUT = ./print_int.txt
EXEC = fib
OPTION =
SIMULATOR_OPT = -b ./breakpoints.txt

minrt.s: ./src/minrt/minrt.ml
	mkdir -p ./build/
	stack run -- -i ../stdlib/float.ml -i ./src/minrt/globals.ml -i $^ -o ./build/$@ --emit-all --verbose
	cp ./build/$@ .

%.s: ./src/%.ml
	mkdir -p ./build/
	stack run -- $(OPTION) -i $^ -o ./build/$@ --emit-all --verbose
	cp ./build/$@ .

%.bin: %.s
	mkdir -p ./build/
	$(ASSEMBLER) -input $^ -output ./build/$(basename $@ .bin)
	cp ./build/$@ .

run: $(EXEC).bin
	rm -f $(OUTPUT)
	cp ./build/$(EXEC)_labels.csv ./labels.csv
	$(SIMULATOR) -i $^ $(SIMULATOR_OPT)

clean:
	rm -rf ./build/
	rm -f *.s
	rm -f *.bin
	rm -f $(OUTPUT)
