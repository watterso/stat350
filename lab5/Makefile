all: tex pdf

pdf: 
	cd bin && pdflatex layout.tex
	cp bin/layout.pdf watterso_lab5.pdf

tex:
	mkdir -p bin
	cd bin && r -e "library(knitr);knit('../layout.Rnw')"

clean:
	rm -rf watterso_lab5.pdf
	rm -rf bin 
