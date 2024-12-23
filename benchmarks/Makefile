.PHONY: run clean
.PRECIOUS: %.s

ASSEMBLER = ./bin/r72b
SIMULATOR = ./bin/r7sim
OUTPUT = ./print_int.txt
EXEC = fib
OPTION =
SIMULATOR_OPT = -b ./breakpoints.txt

include ./src/minrt/minrt.mk

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
