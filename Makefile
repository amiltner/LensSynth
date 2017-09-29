.PHONY: all clean regenerate-data

all:
	make -C program
	make -C paper

clean:
	make -C program clean
	make -C paper clean

regenerate-data:
	make -C program generate-data
	make -C comparisons/prose/ generate-data
	./combine-data.py program/generated_data/data.csv comparisons/prose/Extraction.Text/generated_data/data.csv comparisons/prose/Transformation.Text/generated_data/data.csv
	make -C papers/BijectiveLensSynth regenerate-figures

regenerate-specs:
	make -C program generate-io-specs
	make -C program generate-extraction-specs
	rm -rf comparisons/io_specs
	rm -rf comparisons/extraction_specs
	mv program/generated_io_specs/example-inputs comparisons/io_specs
	mv program/generated_extraction_specs/example-inputs comparisons/extraction_specs
