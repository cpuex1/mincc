RENDER = contest.ppm

mandelbrot.s: ./src/mincaml/mandelbrot.ml
	mkdir -p $(BUILD_DIR)
	$(MINCC) $(MINCC_OPTION) -i $^ -o $(BUILD_DIR)/$@
	cp $(BUILD_DIR)/$@ .
