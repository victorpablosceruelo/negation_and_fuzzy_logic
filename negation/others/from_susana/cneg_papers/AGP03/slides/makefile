file = slidesAGP02

$(file).pdf : $(file).ps
	ps2pdf $(file).ps

$(file).ps : $(file).dvi
	dvips -o $(file).ps $(file).dvi 

$(file).dvi : $(file).tex
	latex $(file).tex


view : $(file).pdf
	xpdf $(file).pdf &

show : $(file).pdf
	acroread $(file).pdf
