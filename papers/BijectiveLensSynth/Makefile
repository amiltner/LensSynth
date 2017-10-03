.PHONY : main clean

main:
	latexmk -pdf main.tex

watch :
	latexmk -pdf -pvc main.tex

clean :
	latexmk -C

regenerate-figures:
	python /transform-data.py generated-data/data.csv

bcp:
	cp $(HOME)/common/bib/bcp.bib .
	-pdflatex main
	-bibtex main
