RENDER = contest.ppm

minrt.s: ./src/minrt/minrt.ml
	mkdir -p $(BUILD_DIR)
	$(MINCC) $(MINCC_OPTION) -i ../stdlib/float.ml -i ./src/minrt/globals.ml -i $^ -o $(BUILD_DIR)/$@
	cp $(BUILD_DIR)/$@ .

%.ppm: minrt.bin
	rm -f print_int.txt
	cp ./src/minrt/$(basename $@).sld ./read_int.txt
	printf "2\nc\n" | $(SIM) -i $^ $(SIM_OPTION)
	mv print_int.txt $@

minrt:
	rm -f $(RENDER)
	make $(RENDER)
	python3 ./notifier.py $(RENDER)
