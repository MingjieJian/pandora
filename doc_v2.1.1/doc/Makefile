all: dvi ps pdf
dvi: 
	tex wup
	latex paper
ps: dvi
	dvips -o wup.ps wup
	dvips -o paper.ps paper
pdf: 
	pdftex wup
	pdflatex paper
clean:
	rm *.aux *.log
