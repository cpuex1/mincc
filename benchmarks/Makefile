.PHONY: run clean minrt
.PRECIOUS: %.s

# Tools
MINCC = stack run --
ASM = ./bin/r72b
SIM = ./bin/r7sim

# Tool options
MINCC_OPTION = --emit-all --verbose
ASM_OPTION =
SIM_OPTION = -b ./breakpoints.txt

# Build options
BUILD_DIR = ./build

# Configurations
EXEC = fib

# Include minrt targets
include ./src/minrt/minrt.mk

%.s: ./src/%.ml
	mkdir -p $(BUILD_DIR)
	$(MINCC) $(MINCC_OPTION) -i $^ -o $(BUILD_DIR)/$@
	cp $(BUILD_DIR)/$@ .

%.bin: %.s
	mkdir -p $(BUILD_DIR)
	$(ASM) $(ASM_OPTION) -input $^ -output $(BUILD_DIR)/$(basename $@ .bin)
	cp $(BUILD_DIR)/$@ .

run: $(EXEC).bin
	rm -f print_int.txt
	cp $(BUILD_DIR)/$(EXEC)_labels.csv ./labels.csv
	$(SIM) -i $^ $(SIM_OPTION)

clean:
	rm -rf $(BUILD_DIR)
	rm -f *.s
	rm -f *.bin
