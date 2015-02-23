all: tex pdf

pdf: 
	cd bin && pdflatex layout.tex
	cp bin/layout.pdf graphs.pdf

tex:
	mkdir -p bin
	cd bin && r -e "library(knitr);knit('../layout.Rnw')"

clean:
	rm -rf graphs.pdf
	rm -rf bin 
