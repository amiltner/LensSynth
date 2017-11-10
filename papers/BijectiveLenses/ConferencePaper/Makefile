.PHONY : main clean

main:
	latexmk -pdf main.tex

regenerate-graphs:
	python transform-data.py generated-data/data.csv

watch :
	latexmk -pdf -pvc main.tex

clean :
	latexmk -C

bcp:
	cp $(HOME)/common/bib/bcp.bib .
	-pdflatex main
	-bibtex main
