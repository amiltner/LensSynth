.PHONY: all clean regenerate-data

all:
	make -C program
	make -C papers

clean:
	make -C program clean
	make -C papers clean

regenerate-data:
	make -C program generate-data
	cp program/generated_data/data.csv paper/generated-data/data.csv
	make -C papers
