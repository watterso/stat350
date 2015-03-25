all: tex pdf

pdf: 
	cd bin && pdflatex layout.tex
	cp bin/layout.pdf output.pdf

tex:
	mkdir -p bin
	cd bin && r -e "library(knitr);knit('../layout.Rnw')"

clean:
	rm -rf output.pdf
	rm -rf bin 
