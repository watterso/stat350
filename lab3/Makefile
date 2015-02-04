all: tex pdf
	rm -rf temp

pdf: 
	cd temp && pdflatex lab3.tex
	cp temp/lab3.pdf .

tex:
	mkdir temp
	cd temp && r -e "library(knitr);knit('../lab3.Rnw')"

clean:
	rm -rf lab3.pdf
	rm -rf temp 
