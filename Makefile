.PHONY: all clean regenerate-data

all:
	make -C paper

clean:
	make -C program clean
	make -C paper clean

regenerate-data:
	make -C program generate-data
	cp program/generated_data/data.csv paper/generated_data/data.csv
	make -C paper
