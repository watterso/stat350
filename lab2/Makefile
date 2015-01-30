all: tex pdf
	rm -rf temp

pdf: 
	cd temp && pdflatex lab2.tex
	cp temp/lab2.pdf .

tex:
	mkdir temp
	cd temp && r -e "library(knitr);knit('../lab2.Rnw')"

clean:
	rm -rf lab2.pdf
	rm -rf temp 
