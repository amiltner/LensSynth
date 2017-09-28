.PHONY: all clean regenerate-data

all:
	make -C program
	make -C paper

clean:
	make -C program clean
	make -C paper clean

regenerate-data:
	make -C program generate-data
	cp program/generated_data/data.csv paper/generated-data/data.csv
	make -C paper

regenerate-specs:
	make -C program generate-io-specs
	make -C program generate-extraction-specs
	mv program/generated_io_specs comparisons/io_specs
	mv program/generated_extraction_specs comparisons/extraction_specs
