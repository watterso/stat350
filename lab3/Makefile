all: tex pdf D


D:
	mkdir temp1
	cd temp1 && r -e "library(knitr);knit('../lab3_d.Rnw')"
	cd temp1 && pdflatex lab3_d.tex
	cp temp1/lab3_d.pdf .

pdf: 
	cd temp && pdflatex lab3.tex
	cp temp/lab3.pdf .

tex:
	mkdir temp
	cd temp && r -e "library(knitr);knit('../lab3.Rnw')"

cleanD:
	rm -rf temp1 
	rm -rf lab3_d.pdf

clean:
	rm -rf lab3.pdf
	rm -rf lab3_d.pdf
	rm -rf temp 
	rm -rf temp1 
