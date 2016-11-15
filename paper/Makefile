.PHONY : main clean

main:
	latexmk -pdf main.tex

watch :
	latexmk -pdf -pvc main.tex

clean :
	latexmk -C

bcp:
	cp $(HOME)/common/bib/bcp.bib .
	-pdflatex main
	-bibtex main
