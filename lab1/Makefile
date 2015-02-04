all: pdf
	rm -rf temp

pdf: tex
	cd temp && pdflatex lab1.tex
	cp temp/lab1.pdf .

tex:
	mkdir temp
	cd temp && r -e "library(knitr);knit('../lab1.Rnw')"

clean:
	rm -rf lab1.pdf
	rm -rf temp 
