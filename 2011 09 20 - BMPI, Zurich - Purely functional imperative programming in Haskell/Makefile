# Create LaTeX beamer slides via pandoc
# #####################################
#
# Setup based on Keegan Mc'Allisters blog-post
#
#   http://mainisusuallyafunction.blogspot.com/2011/08/new-slides-and-how-i-made-them.html
#

slides: 
	pandoc -f markdown -t latex --template beamer.template -i presentation.markdown -o presentation.tex -H slides_preamble.tex --listings
	perl -pe 's/\\section/\\end{frame}\n\\begin{frame}[fragile]/' -i presentation.tex
	perl -pe 's/\\end{document}/\\end{frame}\n\\end{document}/' -i presentation.tex
	pdflatex presentation.tex </dev/null
	# bibtex presentation
	# pdflatex presentation.tex </dev/null
