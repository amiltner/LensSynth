.PHONY : main clean

main:
	latexmk -pdf main.tex

watch :
	latexmk -pdf -pvc main.tex

clean :
	latexmk -C

fast:
	-pdflatex main
	-bibtex main
