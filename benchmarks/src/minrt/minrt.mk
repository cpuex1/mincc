.PRECIOUS: %.s

ASM = ./bin/r72b
SIM= ./bin/r7sim
BUILD_DIR = ./build

MINCC_OPTION = --emit-all --verbose
ASM_OPTION =
SIM_OPTION =

minrt.s: ./src/minrt/minrt.ml
	mkdir -p $(BUILD_DIR)
	stack run -- -i ../stdlib/float.ml -i ./src/minrt/globals.ml -i $^ -o ./build/$@ $(MINCC_OPTION)
	cp $(BUILD_DIR)/$@ .

minrt.bin: minrt.s
	mkdir -p $(BUILD_DIR)
	$(ASM) -input $^ -output $(BUILD_DIR)/minrt $(ASM_OPTION)
	cp $(BUILD_DIR)/$@ .

%.ppm: minrt.bin
	rm -f print_int.txt
	cp ./src/minrt/$(basename $@).sld ./read_int.txt
	printf "2\nc\n" | $(SIM) -i $^ $(SIM_OPTION)
	mv print_int.txt $@
